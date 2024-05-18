// The len handling is obviously screwed.


#include <stdio.h>
#include <stdlib.h>

typedef struct DllNode {
    int val;
    int *len;
    struct DllNode *next;
    struct DllNode *prev;
} DllNode;

DllNode *new_node(int val, int *len);
DllNode *next(DllNode *node);
DllNode *prev(DllNode *node);
int set_prev(DllNode* node, DllNode* ptr);
int set_next(DllNode* node, DllNode* ptr);
int unlink(DllNode* node);

int main() {
    int *len = (int*)malloc(sizeof(int));
    *len = 0;

    DllNode *node1 = new_node(1, len);
    DllNode *node2 = new_node(2, node1->len);
    DllNode *node3 = new_node(3, node1->len);

    set_next(node1, node2);
    set_prev(node2, node1);

    set_next(node2, node3);
    set_prev(node3, node2);

    if (node1->len != NULL) {
        printf("Length of list: %d\n", *node1->len);
    } else {
        printf("List length is NULL\n");
    }

    unlink(node2);

    if (node1->len != NULL) {
        printf("Length of list after unlink: %d\n", *node1->len);
    } else {
        printf("List length is NULL\n");
    }

    unlink(node1);
    unlink(node3);

    return 0;
}


DllNode *new_node(int val, int *len) {
    DllNode *node = (DllNode*)malloc(sizeof(DllNode));
    node->val = val;
    node->len = len;
    node->next = NULL;
    node->prev = NULL;
    return node;
};

DllNode *next(DllNode *node) {
    return node->next;
};

DllNode *prev(DllNode *node) {
    return node->prev;
};

int set_prev(DllNode* node, DllNode* ptr) {
    if (node->prev != NULL) {
        return 1;
    };
    node -> prev = ptr;

    if (node->len == NULL) {
        node->len = (int*)malloc(sizeof(int));
        (*node->len) = 2;

    } else {
        (*node->len)++;
    }
    return 0;
};

int set_next(DllNode* node, DllNode* ptr) {
    if (node->next != NULL) {
        return 1;
    };

    node -> next = ptr;

    if (node->len == NULL) {
        node->len = (int*)malloc(sizeof(int));
        (*node->len) = 2;

    } else {
        (*node->len)++;
    }
    return 0;
};

/// Don't forget to free after the unlink
int unlink(DllNode* node) {

    DllNode *next_ptr = node->next;
    DllNode *prev_ptr = node->prev;
    
    if (next_ptr != NULL) {
        next_ptr->prev = prev_ptr;
    }
    if (prev_ptr != NULL) {
        prev_ptr->next = next_ptr;
    }
    if (*node->len == 1) {
        free(node->len);
    } else {
        (*node->len)--;
    }

    free(node);
    return 1;
};
