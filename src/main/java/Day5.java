import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class Day5 {

    public static void main(String[] args) throws IOException {
            Scanner s = new Scanner(new File("Day5.txt"));
            ArrayList<Integer> input = new ArrayList<>();
            while(s.hasNext()) input.add(Integer.valueOf(s.next()));
            System.out.println(trampoline(input));
    }

    private static Integer trampoline(ArrayList<Integer> arr) {
        int cnt = 0;
        for(int i = 0; 0 <= i && i < arr.size(); cnt++) {
            int jump = arr.get(i);
            int offset = 1;
            if(jump >= 3) offset = -1;
            arr.set(i, arr.get(i) + offset);
            i = i + jump;
        }
        return cnt;
    }

}
