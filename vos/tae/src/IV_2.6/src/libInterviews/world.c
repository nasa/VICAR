/*
 * Copyright (c) 1987, 1988, 1989 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * A world is the root scene for a display.
 */
/*
 * Change Log:
 * 19-mar-93    HP compiler does not like some kinds of initialization...rt
 */

#include <InterViews/canvas.h>
#include <InterViews/event.h>
#include <InterViews/painter.h>
#include <InterViews/propsheet.h>
#include <InterViews/shape.h>
#include <InterViews/strtable.h>
#include <InterViews/world.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

extern StringTable* nameTable;

double inch, inches, cm, point, points;

static OptionDesc defoptions[] = {
    { "-background", "*background", OptionValueNext },
    { "-bg", "*background", OptionValueNext },
    { "-display", "display", OptionValueNext },
    { "-fg", "*foreground", OptionValueNext },
    { "-fn", "*font", OptionValueNext },
    { "-font", "*font", OptionValueNext },
    { "-foreground", "*foreground", OptionValueNext },
    { "-geometry", ".geometry", OptionValueNext },
    { "-iconic", ".iconic", OptionValueImplicit, "on" },
    { "-name", ".name", OptionValueNext },
    { "-reverse", "*reverseVideo", OptionValueImplicit, "on" },
    { "-rv", "*reverseVideo", OptionValueImplicit, "on" },
    { "-title", ".title", OptionValueNext },
    { "-xrm", nil, OptionPropertyNext },
    { nil }
};

static PropDir* appdir;

static void PutProp (
    const char* path, const char* value, const char* type = nil
) {
    if (path[0] == '.' || path[0] == '*') {
	properties->PutLocal(appdir, path, value, type);
    } else {
	properties->Put(path, value, type);
    }
}

static void PutPropLower (
    const char* path, const char* value, const char* type = nil
) {
    if (path[0] == '.' || path[0] == '*') {
	properties->PutLocalLower(appdir, path, value, type);
    } else {
	properties->PutLower(path, value, type);
    }
}

World::World (const char* clssname, int& argc, char* argv[]) {
    Setup(clssname, argc, argv);
    ParseArgs(argc, argv, defoptions);
    Init(GetAttribute("display"));
    LoadUserDefaults();
    FinishInit();
}

World::World (
    const char* clssname, OptionDesc* opts, int& argc, char* argv[]
) {
    Setup(clssname, argc, argv);
    ParseArgs(argc, argv, opts);
    ParseArgs(argc, argv, defoptions);
    Init(GetAttribute("display"));
    LoadUserDefaults();
    FinishInit();
}

World::World (
    const char* clssname, PropertyData* initprops,
    OptionDesc* opts, int& argc, char* argv[]
) {
    Setup(clssname, argc, argv);
    ParseArgs(argc, argv, opts);
    ParseArgs(argc, argv, defoptions);
    Init(GetAttribute("display"));
    LoadUserDefaults();
    for (register PropertyData* p = &initprops[0]; p->path != nil; p++) {
	PutPropLower(p->path, p->value, p->type);
    }
    FinishInit();
}

World::World (const char* instnce, const char* device) {
    char* argv[1];
    const char* clssname = instnce;
    Setup(clssname, 0, argv);
    Init(device);
    LoadUserDefaults();
    FinishInit();
}

/*
 * FindAppInstnce follows the ICCCM rules for defining an application's
 * instance name from the command line or an environment variable.
 */

static const char* FindAppInstnce (int argc, char* argv[]) {
    const char* instnce = nil;

    for (int i = 1; i < argc; i++) {
	if (strcmp("-name", argv[i]) == 0) {
	    i++;
	    if (i < argc) {
		instnce = argv[i];
	    }
	}
    }

    if (instnce == nil) {
	instnce = getenv("RESOURCE_NAME");
    }

    if (instnce == nil && argc > 0) {
	char* ptr = strrchr(argv[0], '/');
	instnce = (ptr) ? ++ptr : argv[0];
    }

    return instnce;
}

void World::Setup (const char* clssname, int argc, char* argv[]) {
    if (properties == nil) {
	properties = new PropertySheet;
    }
    if (nameTable == nil) {
	nameTable = new StringTable(1024);
    }

    const char* instnce = FindAppInstnce(argc, argv);
    appdir = (instnce == nil) ?
	properties->Root() : properties->MakeDir(instnce);

    SaveCommandLine(argc, argv);
    SetClassName(clssname);
    SetInstance(instnce);
}

void World::LoadUserDefaults () {
    const char* defaults = UserDefaults();
    if (defaults != nil) {
	properties->LoadList(defaults);
    } else {
	char* filename = getenv("XENVIRONMENT");
	if (filename == nil) {
	    filename = getenv("HOME");
	    if (filename != nil) {
		char buf[1024];

		sprintf(buf, "%s/.Xdefaults", filename);
		properties->LoadFile(buf);
	    }
	} else {
	    properties->LoadFile(filename);
	}
    }
}

static void BadArg (const char* fmt, const char* arg) {
    fflush(stdout);
    fprintf(stderr, fmt, arg);
    putc('\n', stderr);
    exit(1);
}

void World::ParseArgs (int& argc, char* argv[], OptionDesc opts[]) {
    register int i;
    register OptionDesc* o;
    int newargc;
    char* newargv[1024];
    boolean match;

    newargc = 1;
    newargv[0] = argv[0];
    for (i = 1; i < argc; i++) {
	match = false;
	char* arg = argv[i];
	for (o = &opts[0]; o->name != nil; o++) {
	    if (strcmp(o->name, arg) == 0) {
		match = true;
		switch (o->style) {
		    case OptionPropertyNext:
			++i;
			if (i == argc) {
			    BadArg("missing property after '%s'", arg);
			}
			arg = argv[i];
        {
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
				char* value;
				value = strchr(arg, ':');
#else

				char* value = strchr(arg, ':');
#endif
				if (value == nil) {
				    BadArg("missing ':' in '%s'", arg);
				}
				*value = '\0';
				PutProp(arg, value+1);
        }
			break;
		    case OptionValueNext:
			++i;
			if (i == argc) {
			    BadArg("missing value after '%s'", arg);
			}
			PutProp(o->path, argv[i]);
			break;
		    case OptionValueImplicit:
			PutProp(o->path, o->value);
			break;
		    case OptionValueIsArg:
			PutProp(o->path, arg);
			break;
		    case OptionValueAfter:
			BadArg("missing value in '%s'", arg);
			break;
		}
	    } else if (o->style == OptionValueAfter) {
		int len = strlen(o->name);
		if (strncmp(o->name, arg, len) == 0) {
		    match = true;
		    PutProp(o->path, &arg[len]);
		}
	    }
	}
	if (!match && opts == defoptions) {
	    if (arg[0] == '=') {
		match = true;
		PutProp(".geometry", &arg[1]);
	    } else if (strchr(arg, ':') != nil) {
		match = true;
		PutProp("display", arg);
	    }
	}
	if (!match) {
	    newargv[newargc] = arg;
	    ++newargc;
	}
    }
    if (newargc < argc) {
	for (i = 1; i < newargc; i++) {
	    argv[i] = newargv[i];
	}
	argc = newargc;
	argv[argc] = nil;
    }
}

/*
 * It makes no sense to insert a popup interactor without a position
 * because the user can't place a popup interactor (such an interactor
 * never interacts with window managers), but we provide this version of
 * InsertPopup for consistency.  If we don't find a geometry resource
 * for the popup interactor, we'll center it in the middle of the screen.
 */

void World::InsertPopup (Interactor* i) {
    Coord x = 0;
    Coord y = 0;
    unsigned int width = i->shape->width;
    unsigned int height = i->shape->height;
    unsigned int u = GetGeometry(i, x, y, width, height);
    if ((u & (GeomXValue|GeomYValue)) != 0) {
	InsertPopup(i, x, y);
    } else {
	InsertPopup(i, xmax/2, ymax/2, Center);
    }
}

void World::InsertPopup (Interactor* i, Coord x, Coord y, Alignment a) {
    i->SetInteractorType(PopupInteractor);
    Insert(i, x, y, a);
}

void World::InsertTransient (Interactor* i, Interactor* owner) {
    i->SetInteractorType(TransientInteractor);
    i->SetGroupLeader(owner);
    i->SetTransientFor(owner);
    Insert(i);
}

void World::InsertTransient (
    Interactor* i, Interactor* owner, Coord x, Coord y, Alignment a
) {
    i->SetInteractorType(TransientInteractor);
    i->SetGroupLeader(owner);
    i->SetTransientFor(owner);
    Insert(i, x, y, a);
}

void World::InsertToplevel (Interactor* i, Interactor* leader) {
    i->SetInteractorType(ToplevelInteractor);
    i->SetGroupLeader(leader);
    Insert(i);
}

void World::InsertToplevel (
    Interactor* i, Interactor* leader, Coord x, Coord y, Alignment a
) {
    i->SetInteractorType(ToplevelInteractor);
    i->SetGroupLeader(leader);
    Insert(i, x, y, a);
}

void World::InsertApplication (Interactor* i) {
    i->SetInteractorType(ApplicationInteractor);
    Insert(i);
}

void World::InsertApplication (
    Interactor* i, Coord x, Coord y, Alignment a
) {
    i->SetInteractorType(ApplicationInteractor);
    Insert(i, x, y, a);
}

void World::InsertIcon (Interactor* i) {
    i->SetInteractorType(IconInteractor);
    Insert(i);
}

void World::InsertIcon (
    Interactor* i, Coord x, Coord y, Alignment a
) {
    i->SetInteractorType(IconInteractor);
    Insert(i, x, y, a);
}

/*
 * We override locally defined properties with properties stored by
 * command-line options if this is the first interactor we've ever
 * inserted into the world (otherwise -geometry, etc., wouldn't have
 * any effect.)
 */

static void OverrideLocalProperties (World* world, Interactor* i) {
    static boolean firsttime = true;
    if (firsttime) {
	const char* g = world->GetAttribute("geometry");
	if (g != nil) {
	    i->SetGeometry(g);
	}

	const char* ig = world->GetAttribute("iconGeometry");
	if (ig != nil) {
	    i->SetIconGeometry(ig);
	}

	const char* iconic = world->GetAttribute("iconic");
	if (iconic != nil) {
	    if (strcmp(iconic, "on") == 0) {
		i->SetStartIconic(true);
	    } else if (strcmp(iconic, "off") == 0) {
		i->SetStartIconic(false);
	    }
	}

	const char* t = world->GetAttribute("title");
	if (t != nil) {
	    i->SetName(t);
	}
    }
    firsttime = false;
}

/*
 * Insert an interactor with user placement unless the arguments 
 * or the resources database specify a position.
 */

void World::DoInsert (Interactor* i, boolean b, Coord& x, Coord& y) {
    unsigned int width = i->shape->width;
    unsigned int height = i->shape->height;
    OverrideLocalProperties(this, i);
    if (b && width > 0 && height > 0) {
	Place(i, x, y, x + width - 1, y + height - 1);
    } else {
	unsigned int u = GetGeometry(i, x, y, width, height);
	if ((u & (GeomXValue|GeomYValue)) != 0 && width > 0 && height > 0) {
	    Place(i, x, y, x + width - 1, y + height - 1);
	} else {
	    UserPlace(i, width, height);
	}
    }
    FinishInsert(i);
}

/*
 * GetGeometry returns left, bottom, width, and height.
 */

unsigned int World::GetGeometry (
    Interactor* i, Coord& x, Coord& y, unsigned int& w, unsigned int& h
) {
    unsigned int r = GeomNoValue;
    const char* g = i->GetGeometry();
    if (g != nil) {
	r = ParseGeometry(g, x, y, w, h);
	if ((r & GeomXValue) != 0) {
	    if ((r & GeomXNegative) != 0) {
		x = xmax + 1 + x - w;
	    }
	} else {
	    x = 0;
	}
	if ((r & GeomYValue) != 0) {
	    if ((r & GeomYNegative) != 0) {
		y = -y;
	    } else {
		y = ymax + 1 - y - h;
	    }
	} else {
	    y = ymax + 1 - h;
	}
    }
    return r;
}

int World::Width () {
    return canvas->width;
}

int World::Height () {
    return canvas->height;
}

Coord World::InvMapX (Coord x) {
    return x;
}

Coord World::InvMapY (Coord y) {
    return canvas->height - 1 - y;
}
