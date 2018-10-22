#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "cartoLinkedList.h"

/*********************************************/
LINKEDLIST* LinkedList_getLinkedList()
{
   LINKEDLIST *list;
   list = (LINKEDLIST*)malloc(sizeof(LINKEDLIST));
   list->size = 0;

   return list;
}

/*********************************************/
void LinkedList_addNode(LINKEDLIST *list, struct node *n)
{
   if(!(list->size))
   {
      n->next = n;
      n->prev = n;
      list->head = n;
      list->tail = n;
      ++(list->size);

      return;
   }

   n->next = NULL;
   n->prev = list->tail;
   list->tail->next = n;
   list->tail = n;
   n->next = NULL;

   ++(list->size);
}

/*********************************************/
void LinkedList_setNodeArray(LINKEDLIST *list, struct node** array)
{
   int i;

   //   printf("setNodeArray1 %d\n", list->size);
   array[0] = list->head;
   //   printf("setNodeArray2\n");
   for(i = 1; i < list->size; i++)
   {
      array[i] = array[i-1]->next;
      //      printf("setNodeArray3 %d %x %x\n", i, array[i-1], array[i]);
   }
}

/*********************************************/
struct node* LinkedList_addCommon(LINKEDLIST *list, void *data)
{
   struct node *n = (struct node*)malloc(sizeof(struct node));
   n->data = data;

   LinkedList_addNode(list, n);

   return n;
}

/*********************************************/
void LinkedList_addWithRank(LINKEDLIST *list, void *data, int rank)
{
   struct node *n = LinkedList_addCommon(list, data);
   n->rank = rank;
}

/*********************************************/
void LinkedList_add(LINKEDLIST *list, void *data)
{
   struct node *n = LinkedList_addCommon(list, data);
   n->rank = -1;
}

/*********************************************/
void LinkedList_removeNode(LINKEDLIST *list, struct node *n)
{
   //   printf("list: %x %d\n", list, list->size);
   if(list->size == 1)
   {
      list->head = NULL;
      list->tail = NULL;
   }
   else if(list->head == n)
   {
      //      printf("inside removedNode 1\n");
      list->head = list->head->next;
      //      printf("inside removedNode 2\n");
      list->head->prev = list->tail;
      //      printf("inside removedNode 3\n");
      list->tail->next = NULL;
   }
   else if(list->tail == n)
   {
      list->tail = list->tail->prev;
      list->tail->next = NULL;
      list->head->prev = list->tail;
   }
   else
   {
     //      printf("1 inside removeNode: n->prev->next: %x n->next->prev: %x\n", n->prev->next, n->next->prev);
      n->prev->next = n->next;
      n->next->prev = n->prev;
      //      printf("2 inside removeNode: n->prev->next: %x n->next->prev: %x\n", n->prev->next, n->next->prev);
   }

   n->next = NULL;
   n->prev = NULL;
   --(list->size);
}

/*********************************************/
void LinkedList_addLinkedLists(LINKEDLIST *list1, LINKEDLIST *list2)
{
   if(!(list1->size) && list2->size)
   {
      list1->head = list2->head;
      list1->tail = list2->tail;
   }
   else
   {
      list1->tail->next = list2->head;
      if(list2->size)
      {
         list2->head->prev = list1->tail;
         list1->tail = list2->tail;
      }
   }
   list1->size += list2->size;
   list2->head = NULL;
   list2->tail = NULL;
   list2->size = 0;
}

/*********************************************/
void LinkedList_free(LINKEDLIST **list)
{
   struct node *n, *next;

   if((*list)->size)
   {
      n = (*list)->head;
      next = n->next;
      while(next != NULL)
      {
         free(n);
         n = next;
         next = next->next;
      }
      free(n);
   }

   free(*list);
}

/*********************************************/
struct node* LinkedList_getMinNode(LINKEDLIST *list)
{
   int i;
   struct node *minNode;
   struct node *n;

   assert(list->size);

   n = list->head;
   minNode = n;
   assert(minNode->rank > -1);
   for(i = 1; i < list->size; i++)
   {
      n = n->next;
      if(n->rank < minNode->rank)
      {
         minNode = n;
         assert(minNode->rank > -1);
      }
   }

   return minNode;
}

/*********************************************/
LINKEDLIST* LinkedList_sortAscending(LINKEDLIST **list)
{
   int i, size;
   LINKEDLIST *sortedList;

   size = (*list)->size;
   sortedList = LinkedList_getLinkedList();
   for(i = 0; i < size; i++)
   {
      struct node *n = LinkedList_getMinNode(*list);
      LinkedList_removeNode(*list, n);
      LinkedList_addNode(sortedList, n);
   }

   LinkedList_free(list);
   return sortedList;
}

/*********************************************/
LINKEDLIST* LinkedList_bigMemSortAscending(LINKEDLIST **list)
{
   int i, listSize;
   int minRank, maxRank, nRank;
   LINKEDLIST **lists;
   LINKEDLIST *sortedList;
   struct node *n;

   n = (*list)->head;
   minRank = n->rank;
   maxRank = n->rank;
   for(i = 1; i < (*list)->size; i++)
   {
      int rank;

      n = n->next;
      rank = n->rank;

      if(minRank > rank) minRank = rank;
      if(maxRank < rank) maxRank = rank;
   }
   nRank = maxRank - minRank + 1;

   lists = (LINKEDLIST**)malloc(sizeof(LINKEDLIST*)*nRank);
   for(i = 0; i < nRank; i++) lists[i] = LinkedList_getLinkedList();

   listSize = (*list)->size;
   for(i = 0; i < listSize; i++)
   {
      struct node *n = (*list)->head;
      LinkedList_removeNode((*list), n);

      LinkedList_addNode(lists[n->rank - minRank], n);
   }

   sortedList = LinkedList_getLinkedList();
   for(i = 0; i < nRank; i++)
   {
      LinkedList_addLinkedLists(sortedList, lists[i]);
      LinkedList_free(&(lists[i]));
   }

   LinkedList_free(list);
   sortedList->tail->next = NULL;
   return sortedList;
}
