import java.io.*;

public class java {
    static void java(int n) {
        for (int i = 1; i < n; i++) {
            for (int j = 0; j < n - i; j++) System.out.print(" ");
            if (i == 1) {
                System.out.println("*");
            } else {
                System.out.print("*");
                for (int j = 0; j < 2*i - 3; j++) System.out.print(" ");
                System.out.println("*");
            }
        }
        for (int j = 0; j < 2*n - 1; j++) System.out.print("*");
        System.out.println();
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        int n = Integer.parseInt(br.readLine().trim());
        br.close();
        java(n);
    }
}
