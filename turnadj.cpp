// turnadj.cpp : Defines the entry point for the console application.
//


#include "stdafx.h"

/* AN INTERACTIVE POST-PROCESSOR FOR PERFORMING HAND ADJUSTMENTS 
	Originally created by Jun Ma and David Tallent for the Unix system.
	upgrade to Windows platform in 2013/2014 by Hu Dong, City of Bellevue, WA

	version 1.0: duplicate of the turnadj in Unix
	version 1.1: expand the 300 intersection limits to 1000.
	version 1.2: edit the title info. Remove Krishnan Saranathan's name, per Dave Tellent's comments.

*/

/* 03/20/94 */

/* Krishnan Saranathan */
#include <string>
using namespace std;
#include "windows.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#define TOTINT 1000 // maximum number of intersections allowed in the post processing
#define NTRNS 12
#define ONEWAY 50

FILE *pt_base, *pt_9393, *pt_93fn, *pt_frat, *pt_out, *pt_add, *pt_node, *pt_interact;
FILE *fp_scrn;
double fratar[TOTINT + 1][NTRNS + 1];
double t9393[TOTINT + 1][NTRNS + 1], t93fn[TOTINT + 1][NTRNS + 1], fin_inc[TOTINT + 1][NTRNS + 1];
double fin_prop[TOTINT + 1][NTRNS + 1], final[TOTINT + 1][NTRNS + 1], base[TOTINT + 1][NTRNS + 1];
int ok_flag[NTRNS + 1], inc_flag[NTRNS + 1], i_act[TOTINT + 1], arr_oneway[ONEWAY][NTRNS + 1];
char flag_c[NTRNS + 1], prnt_fname[125];
int OPT;
int gTotAvailInt = 0; // total number of intersections acually coded in the address file. Assumes address file and other input files are consistent.
int abs_dif, rel_dif, counter = 1;
int for_tab, flag, err_level[10];
int int_act_arr[TOTINT + 1], n_oneway, scrn_id = 0;
char space6[10];
char int_add[TOTINT + 1][50], f_name[10][100];
void increment(int in);
void hand_adj(int num);
void interact(int in, int flg);
void read_fl1();
void read_fl2();
void read_fl3();
void read_fl4();
void display(int num, int in);
void setleng(char str[], int size, int leng);
void ito_a(int n, char s[]);
void reverse(char s[]);
void tab_set(int i);
void data_fls(int num);
void address();
void int_nodes();
void check(int in);
void error_mesg(int num);
void optional();
void fin_print();
void head();
FILE* file_open(char f_name[40], char mode[2]);
errno_t file_open(FILE* file, char f_name[40], char mode[2]);
void prnt_setup(double num);
void heading();
void banned();


typedef struct intersections
{
	int n[6];
}intersect;
intersect inter[TOTINT + 1];

int BEG;

void increment(int in)
{
	int j;
	double bin1, bin2;
	double delta, frat_chk, ratio, abs_del, abs_frat;
	int prop = 0;
	int inflex = 0;
	int increment = 0;
	int oneway_flag = 0;
	for (j = 1; j <= n_oneway; j++)
		if (in == arr_oneway[j][0])
			oneway_flag = j;
	for (j = 1; j <= NTRNS; j++)
	{
		inc_flag[j] = 0;
		ok_flag[j] = 0;
		delta = t93fn[in][j] - t9393[in][j];
		abs_del = fabs((double)delta);
		if (abs_del == 0)
			bin1 = INFINITY;
		else bin1 = delta / abs_del;
		
		if (base[in][j] == 0 && t9393[in][j] == 0 && t93fn[in][j] > 0)
			fin_inc[in][j] = t93fn[in][j];
		else if (base[in][j] == 0 && t9393[in][j] > 0 && t93fn[in][j] == 0)
			fin_inc[in][j] = 0;
		else if (base[in][j] == 0 && t9393[in][j] > 0 && t93fn[in][j] > 0)
			fin_inc[in][j] = t93fn[in][j];
		else if (base[in][j] > 0 && t9393[in][j] == 0 && t93fn[in][j] == 0)
			fin_inc[in][j] = base[in][j];
		else if (base[in][j] > 0 && t9393[in][j] == 0 && t93fn[in][j] > 0)
			fin_inc[in][j] = base[in][j];
		else if (base[in][j] > 0 && t9393[in][j] > 0 && t93fn[in][j] == 0)
			fin_inc[in][j] = base[in][j];
		else {
			// incremental
			increment = (int)(base[in][j] + delta);
			//fin_inc[in][j]
			// proportional
			ratio = t93fn[in][j] / t9393[in][j];
			prop = (int)(ratio * base[in][j]);

			// inflexion
			ratio = t93fn[in][j] / (base[in][j] + t93fn[in][j]);
			inflex = (int)(base[in][j] * ratio);

			if (increment > inflex)
				fin_inc[in][j] = increment;
			else if (inflex < prop) {
				fin_inc[in][j] = inflex;
				inc_flag[j] = -2;
			}
			else {
				fin_inc[in][j] = prop;
				inc_flag[j] = -1;
			}
		}
		//if (fin_inc[in][j]<0 || (bin1<0 && abs_del>0.8*base[in][j]))
		//{
		//	inc_flag[j] = -1;
		//	fin_inc[in][j] = prop;
		//}

		if (oneway_flag>0 && arr_oneway[oneway_flag][j]<0)
			fin_inc[in][j] = 0;
		frat_chk = fratar[in][j] - base[in][j];
		abs_frat = fabs(frat_chk);
		if (delta == 0)
		{
			if (abs_frat <= 10)
				flag_c[j] = ' ';
			else
			{
				ok_flag[j] = -1;
				flag_c[j] = '?';
			}
			continue;
		}
		if (frat_chk == 0)
		{
			flag_c[j] = ' ';
			continue;
		}

		if (abs_frat == 0)
			bin2 = INFINITY;
		else bin2 = frat_chk / abs_frat;

		if (bin1 != bin2)
		{
			if (abs_frat>10)
			{
				ok_flag[j] = -1;
				flag_c[j] = '?';
			}
			else
				flag_c[j] = '!';
		}
		else
		{
			if (bin1>0)
				flag_c[j] = '+';
			else
				flag_c[j] = '-';
		}
	}
}

void hand_adj(int num)

{
	int i, j, st, endd, ctr_interact = 0;
	/*	if(OPT==3 || OPT==4)
	pt_out=file_open(f_name[7],"a");
	*/	if (num != 0)
	{
		st = num;
		endd = num;
	}
	else
	{
		st = BEG;
		endd = gTotAvailInt;
	}
	for (i = st; i <= endd; i++)
	{
		if (i == 164)
			int test = 8;
		flag = 0;
		for (j = 1; j <= 4; j++)
			err_level[j] = 0;
		increment(i);
		check(i);

		for (j = 1; j <= NTRNS; j++) {
			if (ok_flag[j] == -1)
				flag = -1;
		}
		if (flag == -1)
			err_level[1] = -1;

		if (flag == -1 || num != 0 || OPT == 1 || i_act[ctr_interact] == i)
		{
			if (OPT == 3 || OPT == 4)
			{

				if (OPT == 4 && i_act[ctr_interact] == i)
				{
					ctr_interact++;
					interact(i, -1);
				}
				else
				{
					pt_out = file_open(f_name[7], "a");
					prnt_setup(i*1.0);
					for (j = 1; j <= 12; j++)
						prnt_setup(fin_inc[i][j]);
					fprintf(pt_out, "\n");
					fclose(pt_out);
				}
			}
			else
				interact(i, -1);
		}
		/*#################################*/
		//else
		//{
		//if(i==i_act[ctr_interact])
		//ctr_interact++;
		//}
		

	/*			else
		{
		for(j=1;j<=NTRNS;j++)
		final[i][j]=fin_inc[i][j];
		}*/
		
	}
}

void interact(int in, int flg)
{
	int j, k, p, temp_chk;
	char option[100], option1;
	static int prev_in = 0;
	double *arr;
	arr = (double *)calloc(sizeof(double), (gTotAvailInt + 1));
	if (in != prev_in)
	{
		int_act_arr[counter] = in;
		counter++;
	}
	prev_in = in;
	for (;;)
	{
		display(scrn_id, in);
		if (flg != -1)
		{
			fflush(stdout);
			Sleep(5);
			return;
		}
		printf("\n\n\nSelect one of the four options:");
		printf("\n\n1. Choose Inc/Inflex/Prop method");
		printf("\n2. Choose 93fn values");
		printf("\n3. Choose Frater method");
		printf("\n4. Enter new values for turns");
		printf("\n?. For help/comments");
		printf("\n\nMake your Choice: ");
		strcpy_s(option, "");
		p = 0;
		do
		{
			option[p] = getchar();
			p++;
			if (p>15)
			{
				system("cls");
				printf("\n\n\n\n\t\t\t**** Dont mess with me ****\n\n\n");
				exit(1);
			}
		} while (option[p - 1] != '\n');
		/*		printf("%s\n\n",option);
		Sleep(2);
		*/		switch (option[0])
		{
		case '1':
			arr = &(fin_inc[1][0]);
			break;
		case '2': arr = &(t93fn[1][0]);
			break;
		case '3':
			arr = &(fratar[1][0]);
			break;
		case '4':
			system("cls");
			display(scrn_id, in);
			printf("\n\nInput new values:\n");
			scanf_s("%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &final[in][1],
				&final[in][2], &final[in][3], &final[in][4], &final[in][5],
				&final[in][6], &final[in][7], &final[in][8], &final[in][9],
				&final[in][10], &final[in][11], &final[in][12]);
			break;
		case '?':
			system("cls");
			temp_chk = 0;
			for (k = 1; k <= 4; k++)
			{
				if (err_level[k] == 0)
					continue;
				else
				{
					temp_chk++;
					error_mesg(k);
				}
			}
			if (temp_chk == 0)
				error_mesg(0);
			printf("\n\n\npress ENTER to continue");
			do
			{
				option1 = getchar();
			} while (option1 != '\n');
			interact(in, -1);
			break;
		default:
			break;
		}

		if (option[0] == '?' || (option[0]>'0' && option[0]<'5'))
			break;
	}
	if (option[0]>'0' && option[0]<'5')
	{
		if (option[0] != '4')
		for (j = 1; j <= NTRNS; j++)
			final[in][j] = *(arr + (in - 1)*(NTRNS + 1) + j);
		if (option[0] != '3')
		{
			if (pt_out != NULL)
				fclose(pt_out);
			pt_out = file_open(f_name[7], "a");
			prnt_setup(in*1.0);
			for (j = 1; j <= NTRNS; j++)
				prnt_setup(final[in][j]);
			fprintf(pt_out, "\n");
			fclose(pt_out);
		}
		else
			counter--;
	}
}

//void main() {
//	banned();
//	FILE* file;
//	errno_t err = fopen_s(&file, "TM_restrictions_2030_C9T.prn", "r");
//	if (err != 0)
//		printf("file canont be opened");
//	else {
//		printf("file is opened");
//		char temp[1024];
//		while (!feof(file))
//		{
//			fgets(temp, 1024, file);
//			printf("%s\n", temp);			
//		}
//
//
//		fclose(file);
//}

void main()
{
	FILE* fp_temp;
	int j, p = 0;
	char answ[4];
	char file_n[30], inp_fn[75];
	int prnt_in;
	system("cls");
	heading();
	printf("\n\n\tInput the starting intersection: ");
	scanf_s("%d", &BEG);
	banned();
	system("cls");
	system("cls");
	printf("\n\n\n\n\n\n\t\t\t******** Reading input files ********");
	/*	if(answ[0]=='y' || 'Y')
	data_fls(1);
	else
	*/		data_fls(0);
	address();
	int_nodes();
	read_fl1();
	read_fl2();
	read_fl3();
	read_fl4();
	system("cls");

	/*	if(BEG>1)
	{
	printf("\n\n\tDo you want to append the hand adjusted file (y/n): ");
	scanf("%s",answ);
	}
	*/	system("cls");
	printf("\n\nChoose one of the following:\n\n");
	printf("1. Stop at every intersection\n");
	printf("2. Stop with intersections with errors\n");
	printf("3. Choose Inc/Inflex/Prop whenever applicable and populate with counts of model values if data is missing\n");
	printf("4. Same as Option 3, but stop at requested ID's. Use if you want to see how close volumes match for counts, ELEN, and ELFN. \n");
	printf("5. Scan intersections for printing\n");
	printf("\n\nChoose (1-5): ");
	scanf_s("%d", &OPT);
	if (OPT == 4)
	{
		printf("\n\nName the file with intersection numbers at which\nyou wish the program to stop : ");
		scanf_s("%s", file_n, sizeof(file_n));
		pt_interact = file_open(file_n, "r");
		while (!feof(pt_interact))
		{
			fscanf_s(pt_interact, "%d", &i_act[p]);
			p++;
		}
		fclose(pt_interact);
	}
	if (OPT == 5)
	{
		printf("\nEnter name of the output file: ");
		scanf_s("%s", prnt_fname, sizeof(prnt_fname));
		fp_scrn = file_open(prnt_fname, "w");
		fclose(fp_scrn);
		printf("\n\nDo you have a list of intersections that need to be printed (y/n): ");
		scanf_s("%s", answ, sizeof(answ));
		if (strcmp(answ, "y") == 0 || strcmp(answ, "Y") == 0)
		{
			scrn_id = 1;
			printf("\n\nInput name of the file with this list: ");
			scanf_s("%s", inp_fn, sizeof(inp_fn));
			fp_temp = file_open(inp_fn, "r");

			while (fscanf_s(fp_temp, "%d", &prnt_in) != EOF)
			{
				increment(prnt_in);
				display(scrn_id, prnt_in);
			}
			system("cls");
			printf("\n\n\n\tPrinting Completed!\n\n");
			fclose(fp_temp);
			exit(0);
		}
		else
		{
			for (j = BEG; j <= gTotAvailInt; j++)
			{
				scrn_id = 2;
				increment(j);
				display(scrn_id, j);
				if (scrn_id == 1)
					display(scrn_id, j);
			}
			system("cls");
			printf("\n\n\n\tPrinting Completed!\n\n");
			exit(0);
		}
	}

	printf("\n\n\n\n\t\t\t*** Hand Adjustment in Progress ***");
	Sleep(2);
	hand_adj(0);
	if (OPT == 2)
		optional();
	system("cls");
	printf("\n\n\n\n\t\t*** Output sent to file %s ***\n\n\n", f_name[7]);
}

/*
Read TM volume counts for the base year.
*/
void read_fl1()
{
	int i, j;
	char dummy[200];
	int dum;
	// skip the 3-line header
	for (i = 1; i <= 3; i++)
		fgets(dummy, sizeof(dummy), pt_base);

	//printf("\n");
	int temp[13];
	for (i = 1; i <= gTotAvailInt; i++)
	{
		fscanf_s(pt_base, "%d", &dum);
		//printf("%d ", dum);

		for (j = 1; j <= 12; j++) {
			fscanf_s(pt_base, "%d", &temp[j]);
			//printf("%d ", (int)base[i][j]);
			//printf("%d ", temp[j]); 
			base[i][j] = temp[j];
		}
		//printf("\n");	
	}
	if (!pt_base)
		fclose(pt_base);
}

/*
Read TM volumes from the base year EMME model.
*/
void read_fl2()
{
	int i, j;
	char dummy[200];
	int dum;
	int temp[13];
	for (i = 1; i <= 3; i++)
		fgets(dummy, sizeof(dummy), pt_9393);

	//printf("\n");
	for (i = 1; i <= TOTINT; i++)
	{
		fscanf_s(pt_9393, "%d", &dum);
		//printf("%d ", dum);
		for (j = 1; j <= 12; j++) {
			fscanf_s(pt_9393, "%d", &temp[j]);
			t9393[i][j] = temp[j];
			//printf("%d ", temp[j]);
		}
		//printf("\n");
	}
	if (!pt_9393)
		fclose(pt_9393);
}

/*
Read TM volumes from EMME ELFN model.
*/
void read_fl3()
{
	int i, j;
	char dummy[200];
	int dum;
	int temp[13];
	for (i = 1; i <= 3; i++)
		fgets(dummy, sizeof(dummy), pt_93fn);
	//printf("\n");
	for (i = 1; i <= gTotAvailInt; i++)
	{
		fscanf_s(pt_93fn, "%d", &dum);
		//printf("%d ", dum);
		for (j = 1; j <= 12; j++) {
			fscanf_s(pt_93fn, "%d", &temp[j]);
			t93fn[i][j] = temp[j];
			//printf("%d ", temp[j]);
		}
		//printf("\n");
	}
	if (!pt_93fn)
		fclose(pt_93fn);
}

/*
Read Fratar file. Usually this input file is empty. 
*/
void read_fl4()
{
	int i, j;
	char dummy[200];
	int dum;
	double temp[13];

	for (i = 1; i <= 3; i++) {
		fgets(dummy, sizeof(dummy), pt_frat);
	}
	//printf("\n");
	for (i = 1; i <= gTotAvailInt; i++)
	{
		fscanf_s(pt_frat, "%d", &dum);
		//printf("%d ", dum);
		for (j = 1; j <= 12; j++) {
			if (fscanf_s(pt_frat, "%lf", &temp[j]) == EOF)
				fratar[i][j] = 0;
			else fratar[i][j] = temp[j];
			//printf("%d ", temp[j]);
			
		}
		//printf("\n");
	}
	if (!pt_frat)
		fclose(pt_frat);

}

void display(int num, int in)
{
	int j;
	char flag_s[NTRNS + 1][8], answ[4];
	if (num != 1)
		fp_scrn = stdout;
	else
		fp_scrn = file_open(prnt_fname, "a");
	system("cls");
	fflush(fp_scrn);
	fprintf(fp_scrn, "Check the following intersection (%d): %s\n\n", in, int_add[in]);
	fprintf(fp_scrn, "emme/2 nodes for intersection %d: (%d) %d %d %d %d\n\n",
		in, inter[in].n[1], inter[in].n[2], inter[in].n[3], inter[in].n[4],
		inter[in].n[5]);
	fprintf(fp_scrn, "Base\t");
	for (j = 1; j <= 12; j++)
	{
		for_tab = (int)base[in][j];
		tab_set(0);
	}
	fprintf(fp_scrn, "\nELEN\t");
	for (j = 1; j <= 12; j++)
	{
		for_tab = (int)t9393[in][j];
		tab_set(0);
	}
	fprintf(fp_scrn, "\nELFN\t");
	for (j = 1; j <= 12; j++)
	{
		for_tab = (int)t93fn[in][j];
		tab_set(0);
	}
	fprintf(fp_scrn, "\n\nInc.\t");
	for (j = 1; j <= 12; j++)
	{
		for_tab = (int)fin_inc[in][j];
		tab_set(j);
	}
	fprintf(fp_scrn, "\n\nFratar\t");
	for (j = 1; j <= 12; j++)
	{
		for_tab = (int)fratar[in][j];
		tab_set(0);
	}
	for (j = 1; j <= NTRNS; j++)
	{
		strcpy_s(flag_s[j], "ok");
		if (flag_c[j] != '?')
		{
			flag_s[j][2] = flag_c[j];
			flag_s[j][3] = '\0';
		}
		else
			strcpy_s(flag_s[j], "ERR");
	}
	fprintf(fp_scrn, "\n\n\t");
	for (j = 1; j <= NTRNS; j++)
	{
		setleng(flag_s[j], sizeof(flag_s[j]), 6);
		fprintf(fp_scrn, "%s", flag_s[j]);
	}
	if (num == 2)
	{
		printf("\n\n\n\nDo you want to print this intersection(y/n/e:exit): ");
		scanf_s("%s", answ, sizeof(answ));
		if (strcmp(answ, "y") == 0 || strcmp(answ, "Y") == 0)
			scrn_id = 1;
		else if (strcmp(answ, "e") == 0 || strcmp(answ, "E") == 0)
			exit(0);
	}
	if (num == 1)
	{
		fprintf(fp_scrn, "\n\n");
		fclose(fp_scrn);
	}
}

void setleng(char str[], int size, int leng)
{
	int len, i;
	len = strlen(str);
	for (i = 1; i <= (leng - len); i++)
		strcat_s(str, size, " ");
}

void ito_a(int n, char s[])
{
	int i, sign;
	sign = n;
	if (n<0)
		n = -n;
	i = 0;
	if (n == 0)
	{
		strcpy_s(s, sizeof(s), "0");
		return;
	}
	while (n != 0)
	{
		s[i] = n % 10 + '0';
		i++;
		n = n / 10;
	}
	if (sign<0)
		s[i++] = '-';
	s[i] = '\0';
	reverse(s);
}

void reverse(char s[])
{
	int c, i, j;
	for (i = 0, j = strlen(s) - 1; i<j; i++, j--)
	{
		c = s[i];
		s[i] = s[j];
		s[j] = c;
	}
}

void tab_set(int i)
{
	ito_a(for_tab, space6);
	if (i != 0 && inc_flag[i] == -1)
		strcat_s(space6, sizeof(space6), "*");
	else if (i != 0 && inc_flag[i] == -2)
		strcat_s(space6, sizeof(space6), "#");
	setleng(space6, sizeof(space6), 6);
	fprintf(fp_scrn, "%s", space6);
}

/*
Read turnadj input control file fs_turnadj_1pm.
*/
void data_fls(int num)
{
	FILE *pt_read = NULL;
	int i;
	char file_nam[50];
	//pt_read = file_open("fs_turnadj_1pm", "r");
	printf("\n\nEnter file name: ");
	scanf_s("%s", file_nam, sizeof(file_nam));
	pt_read = file_open(file_nam, "r");

	for (i = 1; i <= 7; i++)
		fscanf_s(pt_read, "%s", f_name[i], sizeof(f_name[i]));
	pt_add = file_open(f_name[1], "r"); //address file
	pt_node = file_open(f_name[2], "r"); // link file
	pt_base = file_open(f_name[3], "r"); // base year counts
	pt_9393 = file_open(f_name[4], "r"); // TM volume from EMME base year model
	pt_93fn = file_open(f_name[5], "r"); // TM volume from EMME ELFN model
	pt_frat = file_open(f_name[6], "r"); // fratar 
	if (num == 1) // output file
		pt_out = file_open(f_name[7], "a");
	else
		pt_out = file_open(f_name[7], "w");
	head();
	fclose(pt_out);
	fclose(pt_read);
}

/*
Read address file.
*/
void address()
{
	int i, num;
	char dummy[100];
	char  str[100];
	char str1[50], str2[50], str3[100];
	for (i = 1; i <= 3; i++)
		fgets(dummy, 100, pt_add);
	while (!feof(pt_add))
	{
		gTotAvailInt++;  // how many intersections coded in address
		if (gTotAvailInt > TOTINT) {
			printf_s("Total intersections exceeds maximum allowed.");
			Sleep(3000);
			exit(1);
		}
		strcpy_s(str1, "");
		strcpy_s(str2, "");
		strcpy_s(str3, "");
		fgets(str, 100, pt_add);
		sscanf_s(str, "%d", &num);
		for (i = 0; i <= 15; i++)
		{
			str1[i] = str[5 + i];
			str2[i] = str[31 + i];
		}
		str1[16] = str2[16] = '\0';
		strcat_s(str1, "-");
		strcpy_s(str3, str1);
		strcat_s(str3, str2);
		strcpy_s(int_add[num], str3);
	}
}

/* load link file.
*/
void int_nodes()
{
	int num, i;
	char dummy[100];
	for (i = 1; i <= 3; i++)
		fgets(dummy, 100, pt_node);
	// num is actually the intersection ID from link file. It assumes intersection ID has to be
	// continuous from 1.
	while (!feof(pt_node))
	{
		fscanf_s(pt_node, "%d", &num);
		fscanf_s(pt_node, "%d %d %d %d %d", &inter[num].n[1], &inter[num].n[2],
			&inter[num].n[3], &inter[num].n[4], &inter[num].n[5]);
	}
}

void check(int in)
{
	int i;
	double diff, sigma_frat, sigma_inc, sigma_base;
	double d;
	diff = 0;
	sigma_frat = 0;
	sigma_inc = 0;
	sigma_base = 0;
	for (i = 1; i <= NTRNS; i++)
	{
		diff += fabs((double)(fratar[in][i] - fin_inc[in][i]));
		sigma_frat += fratar[in][i];
		sigma_inc += fin_inc[in][i];
		sigma_base += base[in][i];
	}
	d = sigma_frat - sigma_inc;
	d = fabs(d);
	abs_dif = (int)diff;
	rel_dif = (int)d;
	if (d>20)
	{
		err_level[2] = -1;
		flag = -1;
	}
	if (diff>100)
	{
		err_level[3] = -1;
		flag = -1;
	}
	if (sigma_base == 0)
	{
		err_level[4] = -1;
		flag = -1;
	}
}

void error_mesg(int num)
{
	switch (num)
	{
	case 0:
		printf("This intersection seems okey to me!\n\n\n\n");
		break;
	case 1:
		printf("There is a discrepency between the turning\n");
		printf("movement values obtained by the incremental\n");
		printf("method and the Fratar method.The turn\n");
		printf("movements for which this problem is noticed\n");
		printf("is marked by the flag ERR \n\n");
		break;
	case 2:
		printf("The total arrivals obtained from fratar\n");
		printf("method is different from the arrivals\n");
		printf("obtained by the incremental method.\n");
		printf("This problem occurs when the incremental\n");
		printf("method fails and the propotionality method\n");
		printf("is used instead. These turning movements\n");
		printf("are marked byan asterix (*). (diff=%d)\n\n", rel_dif);
		break;
	case 3:
		printf("The absolute difference between the turning\n");
		printf("movements obtained from the fratar method\n");
		printf("and the incremental method exceeds 100 (=%d)\n\n", abs_dif);
		break;
	case 4:
		printf("There are no base year turning movements\n");
		break;
	default:
		break;
	}
}

void optional()
{
	int intt, check = 0;
	char ans[10];
	do
	{
		system("cls");
		printf("\n\nDo you want to check any particular intersection (y/n): ");
		scanf_s("%s", ans, sizeof(ans));
		if (ans[0] == 'y' || ans[0] == 'Y')
		{
			check++;
			printf("\n\nEnter Intersection number (1-%d): ", gTotAvailInt);
			scanf_s("%d", &intt);
			if (intt == 0)
				exit(1);
			hand_adj(intt);
		}
	} while (ans[0] == 'y' || ans[0] == 'Y');
	if (check>0)
		fin_print();
}

void fin_print()
{
	int i, j, temp;
	fclose(pt_out);
	pt_out = file_open(f_name[7], "w");
	head();
	for (i = 1; i<counter; i++)
	{
		temp = int_act_arr[i];
		prnt_setup(temp*1.0);
		for (j = 1; j <= NTRNS; j++)
			prnt_setup(final[temp][j]);
		fprintf(pt_out, "\n");
	}
}

void head()
{
	int i;
	fprintf(pt_out, "INT       NB             SB             EB             WB\n");	
	fprintf(pt_out, " #   LT   TH   RT   LT   TH   RT   LT   TH   RT   LT   TH   RT\n");
	for (i = 1; i <= 62; i++)
		fprintf(pt_out, "-");
	fprintf(pt_out, "\n");
}

errno_t file_open(FILE* file_p, char f_name[40], char mode[2])
{
	errno_t err = 0;
	if ((err = fopen_s(&file_p, f_name, mode)) > 0)
	{
		system("cls");
		printf("\n\n\n\n\t\t\t*** Unable to open file %s ***\n\n", f_name);
		exit(err);
	}
	return err;
}

FILE* file_open(char f_name[40], char mode[2])
{
	errno_t err = 0;
	FILE* file_p;
	if ((err = fopen_s(&file_p, f_name, mode)) > 0)
	{
		system("cls");
		printf("\n\n\n\n\t\t\t*** Unable to open file %s ***\n\n", f_name);
		exit(err);
	}
	return file_p;
}

void prnt_setup(double num)
{
	char prnt_str[7];
	int ftoi;
	ftoi = (int)num;
	ito_a(ftoi, prnt_str);
	setleng(prnt_str, sizeof(prnt_str), 5);
	fprintf(pt_out, "%s", prnt_str);
}

void heading()
{
	printf("\n\n\n\n\t\t\t*****************************************\n");
	printf("\t\t\t*                                       *\n");
	printf("\t\t\t*     An Interactive Post-processor     *\n");
	printf("\t\t\t*     to perform Hand Adjustments       *\n");
	printf("\t\t\t*                                       *\n");
	printf("\t\t\t*     Orginally developed by:           *\n");
	printf("\t\t\t*                                       *\n");
	printf("\t\t\t*               David Tallent           *\n");
	printf("\t\t\t*             City of Bellevue          *\n");
	printf("\t\t\t*                 03/20/94              *\n");
	printf("\t\t\t*                                       *\n");
	printf("\t\t\t*    Migrated to Windows Platform by    *\n");
	printf("\t\t\t*           Hu Dong, 2013               *\n");
	printf("\t\t\t*                                       *\n");
	printf("\t\t\t*                                       *\n");
	printf("\t\t\t*****************************************\n");
	Sleep(3000);
	system("cls");
}


void banned()
{
	int i, count = 1;
	char answ[3], file_nam[50];
	FILE *fp_oneway;
	printf("\n\nWill you be using a turn penalty file (y/n): ");
	scanf_s("%s", answ, sizeof(answ));
	if (strcmp(answ, "y") == 0 || strcmp(answ, "Y") == 0)
	{
		
		printf("\n\nEnter turn penalty file name: ");
		scanf_s("%s", file_nam, sizeof(file_nam));
		fp_oneway = file_open(file_nam, "r");
		//file_open(fp_oneway, "TM_restrictions_2030_C9T.prn", "r");
		//fp_oneway = file_open("TM_restrictions_2030_C9T.prn", "r");
		while (!feof(fp_oneway))
		{
			fscanf_s(fp_oneway, "%d", &arr_oneway[count][0]);
			for (i = 1; i <= NTRNS; i++)
				fscanf_s(fp_oneway, "%d", &arr_oneway[count][i]);
			count++;
		}
	}
	n_oneway = --count;
}

