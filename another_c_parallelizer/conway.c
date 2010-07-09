#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

#include "thr.h"

#define NUMROWS  30
#define NUMCOLS  40

int conway_board[NUMCOLS][NUMROWS];

#pragma par_on

void conway_behavior(void)
{
	int nextstatus, sum;
	int row, col;		/* we'll initialize these from main() */
	int row1, col1;

	while (1) {
		/* These threads are all in lock-step, so we let them alternate.
		 * First they all compute their sums and the next statuses, and
		 * then they context switch, and then they each update their
		 * entry in conway_board. Then they context switch again, so that
		 * the values in conway_board are stable while everybody is doing
		 * their sums.
		 */
		sum = 0;
		for (row1 = row - 1; row1 < row + 2; row1++)
			for (col1 = col - 1; col1 < col + 2; col1++) {
				if (!(row1 == row && col1 == col) &&
				    row1 >= 0 && row1 < NUMROWS &&
				    col1 >= 0 && col1 < NUMCOLS)
					sum += conway_board[col1][row1];
			}
		if ((sum == 2 && conway_board[col][row]) || sum == 3)
			nextstatus = 1;
		else
			nextstatus = 0;
		CSW;
		conway_board[col][row] = nextstatus;
		CSW;
	}
}

#pragma par_off

int main(int argc, char *argv[])
{
	int i, j;
	struct thread *thr;
	struct conway_behavior_frame *frame;

	/* set up a thread for each pixel */
	for (j = 0; j < NUMROWS; j++)
		for (i = 0; i < NUMCOLS; i++) {
			thr = new_thread(conway_behavior,
					 sizeof(struct
						conway_behavior_frame));
			frame =
			    (struct conway_behavior_frame *) thr->frame;
			frame->col = i;
			frame->row = j;
		}

	/* make a glider */
	conway_board[2][1] = 1;
	conway_board[3][2] = 1;
	conway_board[1][3] = 1;
	conway_board[2][3] = 1;
	conway_board[3][3] = 1;

	/* make a traffic light */
	conway_board[10][2] = 1;
	conway_board[11][2] = 1;
	conway_board[12][2] = 1;

	while (1) {
		if (argc == 1) {
			/* display pixels normally */
			for (j = 0; j < NUMROWS; j++) {
				for (i = 0; i < NUMCOLS; i++)
					printf("%c",
					       ".*"[conway_board[i][j]]);
				printf("\n");
			}
			printf("---------------\n");
		} else {
			/* Print a 'g' for each generation. This is only good
			 * for showing that generations are quick.
			 */
			printf("g");
			fflush(stdout);
		}
		step_thread(2 * NUMROWS * NUMCOLS);
	}

	return 0;
}
