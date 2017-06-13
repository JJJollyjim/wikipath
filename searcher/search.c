#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <unistd.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define QUEUE_SIZE 100000000
#define BITVEC_SIZE 60000000

struct state {
  uint32_t *map;
  uint32_t *vex;
};

typedef struct queue {
  uint32_t *block_start;
  off_t start;
  off_t end;
} queue;

queue queue_new() {
  uint32_t *start = malloc(QUEUE_SIZE * sizeof(uint32_t));
  if (start == NULL)
    printf("allocation fail");
  queue q = {start, 0, 0};
  return q;
}

void queue_enqueue(queue *q, uint32_t x) {
  *(q->block_start + q->end) = x;
  q->end++;
  if (q->end == QUEUE_SIZE) {
    q->end = 0;
  }
  if (q->end == q->start) {
    printf("q full");
  }
}

uint32_t queue_dequeue(queue *q) {
  uint32_t val = *(q->block_start + q->start);
  q->start++;
  if (q->start == QUEUE_SIZE) {
    q->start = 0;
  }
  return val;
}

off_t queue_size(queue *q) {
  off_t size = q->end - q->start;
  if (size < 0)
    return size = QUEUE_SIZE;
  return size;
}

bool queue_empty(queue *q) {
  return q->end == q->start;
}

void queue_free(queue *q) {
  free(q->block_start);
}


typedef struct bitvec {
  uint64_t *block_start;
} bitvec;

bitvec bitvec_new() {
  uint64_t *start = calloc(BITVEC_SIZE / 64 + 1, sizeof(uint64_t));
  if (start == NULL)
    printf("bitvec allocation fail");
  bitvec v = {start};
  return v;
}

void bitvec_set(bitvec *v, off_t n) {
  *(v->block_start + (n / 64ull)) |= (uint64_t)((uint64_t)1ull << (uint64_t)((uint64_t)n % (uint64_t)64ull));
}

bool bitvec_get(bitvec *v, off_t n) {
  return (((*(v->block_start + (n / 64ull))) & (1ull << (n % 64ull))) != 0ull);
}

void bitvec_free(bitvec *v) {
  free(v->block_start);
}

uint32_t *mapfile(char *path) {
  int fd = open(path, O_RDONLY);
  struct stat stat;
  fstat(fd, &stat);
  return mmap(NULL, stat.st_size, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0);
}

uint32_t *linksfrom(struct state *state, uint32_t source) {
  uint32_t location = state->map[source];

  // 0xFFFFFFFF specifies no links
  return location == 0xFFFFFFFF ? NULL : state->vex + location;
}

void test_q() {
  // tests edge cases iff QUEUE_SIZE == 3
  queue q = queue_new();
  queue_enqueue(&q, 50);
  printf("%d", queue_empty(&q) == false);
  printf("%d", queue_dequeue(&q) == 50);
  printf("%d", queue_empty(&q) == true);
  queue_enqueue(&q, 80);
  queue_enqueue(&q, 81);
  printf("%d", queue_empty(&q) == false);
  printf("%d", queue_dequeue(&q) == 80);
  printf("%d", queue_empty(&q) == false);
  printf("%d", queue_dequeue(&q) == 81);
  printf("%d", queue_empty(&q) == true);
  queue_enqueue(&q, 90);
  printf("%d", queue_dequeue(&q) == 90);
  queue_enqueue(&q, 40);
  queue_enqueue(&q, 41);
  printf("%d", queue_empty(&q) == false);
  printf("%d", queue_dequeue(&q) == 40);
  printf("%d", queue_empty(&q) == false);
  printf("%d", queue_dequeue(&q) == 41);
  printf("%d", queue_empty(&q) == true);
}

void test_bitvec() {
  bitvec v = bitvec_new();

  printf("%d", bitvec_get(&v, 53) == 0);
  bitvec_set(&v, 53);
  printf("%d", bitvec_get(&v, 53) == 1);
  printf("%d", bitvec_get(&v, 54) == 0);
  printf("%d", bitvec_get(&v, 52) == 0);
  printf("%d", bitvec_get(&v, 66) == 0);
  bitvec_set(&v, 66);
  printf("%d", bitvec_get(&v, 53) == 1);
  printf("%d", bitvec_get(&v, 54) == 0);
  printf("%d", bitvec_get(&v, 52) == 0);
  printf("%d", bitvec_get(&v, 66) == 1);
}

void search(struct state *state, uint32_t start, const uint32_t end) {
  queue q = queue_new();
  queue_enqueue(&q, start);

  bitvec visited = bitvec_new();
  bitvec_set(&visited, start);

  uint32_t *parents = calloc(BITVEC_SIZE, sizeof(uint32_t));
  if (parents == NULL) {
    printf("parent alloc fail");
    return;
  }

  int i = 0;

  uint32_t current = 0;
  while (!queue_empty(&q)) {
    i++;
    current = queue_dequeue(&q);

    if (current == end) {
      /* printf("found!!\n"); */
      /* printf("found %" PRIu32 "\n", current); */
      /* printf("found %" PRIu32 "\n", end); */
      while (current != start) {
        printf("%" PRIu32 "\n", current);
        current = *(parents + current);
      }
      printf("%" PRIu32 "\n", current);
      break;
    }

    //printf("%jd\n", (intmax_t)queue_size(&q));

    //if (current == 24413150) {
    //printf("wrjgou");
    //}

    ///if (current == 53934327) {
    ///printf("break\n");
    ///}

    uint32_t *link = linksfrom(state, current);
    if (link != NULL) {
      while (*link != 0) {
        if (!bitvec_get(&visited, *link)) {
          bitvec_set(&visited, *link);
          *(parents + *link) = current;
          queue_enqueue(&q, *link);
        }
        link++;
      }
    }
    //printf("%jd %jd\n", (intmax_t)q.start, (intmax_t)q.end);
  }


  printf("%d\n", i);
  queue_free(&q);
  bitvec_free(&visited);
  free(parents);
}

int main(int argc, char** argv) {
  struct state state = {mapfile("/scratch/vecmap"), mapfile("/scratch/linkVecs")};

  if (argc != 3)
    return 1;

  //test_bitvec();
  search(&state, atoi(argv[1]), atoi(argv[2]));
}

