////////////////////////////////////////////////////////////////////////
// Test program for vectors and quaternions.
////////////////////////////////////////////////////////////////////////

#include "PigVector.h"
#include "PigQuaternion.h"

#include <stdio.h>

int main()
{
    PigVector v1;
    PigVector v2(3.2, 4.5, -2.7);
    double d3[3] = {6.3, -1.4, 3.8};
    PigVector *v3 = new PigVector(d3);

    printf("\nVECTOR TEST\n\n");

    printf("get/set\n");
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("s/b 3.2, 4.5, -2.7\n");

    double x,y,z;
    v3->getXYZ(x, y, z);
    printf("v3: x=%f, y=%f, z=%f\n", x, y, z);
    printf("s/b 6.3, -1.4, 3.8\n");

    double a[3];
    v3->getXYZ(a);
    printf("v3: a[0]=%f, a[1]=%f, a[2]=%f\n", a[0], a[1], a[2]);
    printf("s/b 6.3, -1.4, 3.8\n");

    v1.setX(10.1);
    v1.setY(-8.6);
    v1.setZ(0.4);
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 10.1, -8.6, 0.4\n");

    x=3.1; y=6.6; z=9.3;
    v1.setXYZ(x, y, z);
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 3.1, 6.6, 9.3\n");

    a[0]=3.3; a[1]=4.4; a[2]=5.5;
    v1.setXYZ(a);
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 3.3, 4.4, 5.5\n");

    printf("get/set spherical\n");
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b .95265, -.454787, 6.14654\n");

    v2.setAz(.3312);
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b .3312, -.454787, 6.14654\n");

    v2.setEl(-.0311);
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b .3312, -.0311, 6.146544\n");

    v2.setRange(12.5);
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b .3312, -.0311 12.5 (xyz=11,814944, 4.06276, -.388687\n");

    v2.setSpherical(3.1415926535/8, 3.1415926535/4, 10.0);
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v2: az=%f, el=%f, range=%f\n", v2.getAz(),v2.getEl(),v2.getRange());
    printf("s/b xyz=6.532815, 2.70598, 7.071068\n");

    printf("addition\n");
    PigVector v4 = v1 + *v3;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("v3: x=%f, y=%f, z=%f\n", v3->getX(), v3->getY(), v3->getZ());
    printf("+ : x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 9.6, 3.0, 9.3\n");

    v1 += *v3;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 9.6, 3.0, 9.3\n");

    printf("subtraction\n");
    v4 = v1 - v2;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("- : x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 3.067185, .294019, 2.228932\n");

    v1 -= v2;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("s/b 3.067185, .294019, 2.228932\n");

    printf("cross product\n");
    v4 = v2 * *v3;
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v3: x=%f, y=%f, z=%f\n", v3->getX(), v3->getY(), v3->getZ());
    printf("x : x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 20.18222, 19.723031, -26.1936\n");

    printf("dot product\n");
    double dot = v2 % *v3;
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("v3: x=%f, y=%f, z=%f\n", v3->getX(), v3->getY(), v3->getZ());
    printf(". : dot=%f\n", dot);
    printf("s/b 64.2384\n");

    printf("scalar multiply\n");
    v4 = v1 * 5.0;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("v4: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 15.3359, 1.47009, 11.14466\n");

    v4 *= 10.0;
    printf("v4: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 153.359, 14.7009, 111.4466\n");

    printf("scalar divide\n");
    v4 = v1 / 3.0;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("v4: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b 1.022395, .098006, .742977\n");

    v4 /= 2.0;
    printf("v4: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("s/b .511198, .049003, .371488\n");

    printf("magnitude\n");
    printf("v1.mag=%f, v3.mag=%f, v4.mag=%f\n", v1.magnitude(), v3->magnitude(),
						v4.magnitude());
    printf("s/b 3.802921, 7,489326, .633820\n");

    printf("normalize\n");
    v4.normalize();
    printf("v4 norm: x=%f, y=%f, z=%f\n", v4.getX(), v4.getY(), v4.getZ());
    printf("v4 norm: az=%f, el=%f, range=%f\n", v4.getAz(),v4.getEl(),v4.getRange());
    printf("s/b .806534, .07731, .586111\n");

    v3->normalize();
    printf("v3 norm: x=%f, y=%f, z=%f\n", v3->getX(), v3->getY(), v3->getZ());
    printf("v3 norm: az=%f, el=%f, range=%f\n", v3->getAz(),v3->getEl(),v3->getRange());
    printf("s/b .841197, -.18693, .507389\n");

    delete v3;

    PigVector v6(34.1341345, 55.344112, -42.589971);
    PigVector v7(34.1341401, 55.344105, -42.589980);
// diff:           .0000056    .000007     .000009
    printf("v6: x=%f, y=%f, z=%f\n", v6.getX(), v6.getY(), v6.getZ());
    printf("v7: x=%f, y=%f, z=%f\n", v7.getX(), v7.getY(), v7.getZ());
    printf("Epsilon comparison v6,v7 with 0.000005 (s/b 0): %d\n",
					v6.equalEpsilon(v7,0.000005));
    printf("Epsilon comparison v6,v7 with 0.00001 (s/b 1): %d\n",
					v7.equalEpsilon(v7,0.00001));

////////////////////////////////////////////////////////////////////////

    printf("\nQUATERNION TEST\n\n");

    PigQuaternion q1;
    PigQuaternion q2(.2314, v4);

    printf("get/set\n");
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("s/b .2314, .806534, .07731, .586111\n");

    PigVector v5(0.5, 0.2, 1.0);
    PigQuaternion q3(v5, 3.1415926535/4);
    printf("q3: s=%f, vx=%f, vy=%f, vz=%f\n", q3.getS(), q3.getV().getX(), q3.getV().getY(), q3.getV().getZ());
    printf("s/b .92388, .168467, .067387, .336934\n");

    double a2[3];
    a2[0]=3.2; a2[1]=5.5; a2[2]=1.9;
    PigQuaternion *q4 = new PigQuaternion(2.2, a2);
    printf("q4: s=%f, vx=%f, vy=%f, vz=%f\n", q4->getS(), q4->getV().getX(), q4->getV().getY(), q4->getV().getZ());
    printf("s/b 2.2. 3.2, 5.5, 1.9\n");

    PigQuaternion q5(1.1, 2.2, 3.3, 4.4);
    printf("q5: s=%f, vx=%f, vy=%f, vz=%f\n", q5.getS(), q5.getV().getX(), q5.getV().getY(), q5.getV().getZ());
    printf("s/b 1.1, 2.2, 3.3, 4.4\n");

    double a3[4];
    q2.getComponents(a3);
    printf("q2: a0=%f, a1=%f, a2=%f, a3=%f\n", a3[0], a3[1], a3[2], a3[3]);
    printf("s/b .2314, .806534, .077314, .586111\n");

    PigQuaternion q6;
    a3[0] = .5432; a3[1] = .2211; a3[2] = .7766; a3[3] = .6789;
    q6.setComponents(a3);
    printf("q6: s=%f, vx=%f, vy=%f, vz=%f\n", q6.getS(), q6.getV().getX(), q6.getV().getY(), q6.getV().getZ());
    printf("s/b .5432, .2211, .7766, .6789\n");

    v5 = q2.getU();
    printf("q2: theta=%f, u0=%f, u1=%f, u2=%f\n", q2.getTheta(), v5.getX(), v5.getY(), v5.getZ());
    printf("s/b 2.67456, .829035, .079471, .602462\n");

    q1.setTheta(.431);
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("s/b .97687, .213836, 0, 0\n");

    v5.setXYZ(2.3, -4.1, 1.1);
    q1.setU(v5);
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("s/b .97687, .101868, -.18159, .048719\n");

    v5.setAz(.2213);
    q2.setRotation(v5, .0321);
    printf("v5: x=%f, y=%f, z=%f\n", v5.getX(), v5.getY(), v5.getZ());
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("s/b .999871, .015246, .003430, .003657\n");

    printf("multiply\n");
    q6 = q1 * q2;
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("q6: s=%f, vx=%f, vy=%f, vz=%f\n", q6.getS(), q6.getV().getX(), q6.getV().getY(), q6.getV().getZ());
    printf("s/b .975635, .115917, .177846, .055403\n");

    printf("magnitude\n");
    printf("q4: s=%f, vx=%f, vy=%f, vz=%f\n", q4->getS(), q4->getV().getX(), q4->getV().getY(), q4->getV().getZ());
    printf("q5: s=%f, vx=%f, vy=%f, vz=%f\n", q5.getS(), q5.getV().getX(), q5.getV().getY(), q5.getV().getZ());
    printf("q4.mag=%f, q5.mag=%f\n", q4->magnitude(), q5.magnitude());
    printf("s/b 6.995713, 6.024948\n");

    printf("normalize\n");
    q4->normalize();
    printf("q4n:s=%f, vx=%f, vy=%f, vz=%f\n", q4->getS(), q4->getV().getX(), q4->getV().getY(), q4->getV().getZ());
    printf("s/b .314478, .457423, .786196, .271595\n");

    q5.normalize();
    printf("q5n:s=%f, vx=%f, vy=%f, vz=%f\n", q5.getS(), q5.getV().getX(), q5.getV().getY(), q5.getV().getZ());
    printf("s/b .182574, .365148, .547723, .730297\n");

    printf("rotate vector\n");
    v5 = q1 * v1;
    printf("v1: x=%f, y=%f, z=%f\n", v1.getX(), v1.getY(), v1.getZ());
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("v5: x=%f, y=%f, z=%f\n", v5.getX(), v5.getY(), v5.getZ());
    printf("s/b 2.042821, -.0180548, 3.2076145\n");

    v5 = q2 * v2;
    printf("v2: x=%f, y=%f, z=%f\n", v2.getX(), v2.getY(), v2.getZ());
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("v5: x=%f, y=%f, z=%f\n", v5.getX(), v5.getY(), v5.getZ());
    printf("s/b 6.5622677, 2.5377018, 7.1060987\n");

    printf("prime\n");
    q2 = ~q1;
    printf("q1: s=%f, vx=%f, vy=%f, vz=%f\n", q1.getS(), q1.getV().getX(), q1.getV().getY(), q1.getV().getZ());
    printf("q2: s=%f, vx=%f, vy=%f, vz=%f\n", q2.getS(), q2.getV().getX(), q2.getV().getY(), q2.getV().getZ());
    printf("s/b .976870, -.101868, .181591, -.048719\n");

    PigQuaternion q7(.976865, -.101870, .181590, -.048721);
// diff:             .000005   .000002  .000001, .000002
    printf("q7: s=%f, vx=%f, vy=%f, vz=%f\n", q7.getS(), q7.getV().getX(), q7.getV().getY(), q7.getV().getZ());
    printf("Epsilon comparison q2,q7 with 0.000004 (s/b 0): %d\n",
					q2.equalEpsilon(q7,0.000004));
    printf("Epsilon comparison q2,q7 with 0.000006 (s/b 1): %d\n",
					q2.equalEpsilon(q7,0.000006));

    // Euler test 1
    printf("euler angles 1\n");

    PigQuaternion q8(0.896434, -0.0962536, 0.0873797, 0.423682);
    double roll, pitch, yaw;
    q8.getEulerAngles(roll, pitch, yaw);
    printf("roll=%f, pitch=%f, yaw=%f\n", PigRad2Deg(roll), PigRad2Deg(pitch),
					PigRad2Deg(yaw));
    printf("s/b r=-5.822569, p=13.7816267, y=49.889430\n");
    PigQuaternion q9;
    q9.setEulerAngles(roll, pitch, yaw);
    printf("q9: s=%f, vx=%f, vy=%f, vz=%f\n", q9.getS(), q9.getV().getX(), q9.getV().getY(), q9.getV().getZ());
    printf("s/b 0.896434, -0.0962536, 0.0873797, 0.423682\n");
    printf("Epsilon comparison q8,q9 with 0.000001 (s/b 1): %d\n",
					q8.equalEpsilon(q9,0.000001));

    // Euler test 2
    printf("euler angles 2\n");

    q8 = PigQuaternion(0.929976, 0.0369363, -0.0078811, 0.365675);
    q8.getEulerAngles(roll, pitch, yaw);
    printf("roll=%f, pitch=%f, yaw=%f\n", PigRad2Deg(roll), PigRad2Deg(pitch),
					PigRad2Deg(yaw));
    printf("s/b r=3.611488, p=-2.388312, y=42.855098\n");
    q9.setEulerAngles(roll, pitch, yaw);
    printf("q9: s=%f, vx=%f, vy=%f, vz=%f\n", q9.getS(), q9.getV().getX(), q9.getV().getY(), q9.getV().getZ());
    printf("s/b 0.929976, 0.0369363, -0.0078811, 0.365675\n");
    printf("Epsilon comparison q8,q9 with 0.000001 (s/b 1): %d\n",
					q8.equalEpsilon(q9,0.000001));
}

