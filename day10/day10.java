package day10;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.PriorityQueue;
import java.util.stream.Stream;

public class day10 {
    private static int calculateJoltJumps(List<Integer> values) {
        PriorityQueue<Integer> queue = new PriorityQueue<>(values);
        HashMap<Integer, Integer> diffs = new HashMap<>();
        Integer curJolt = 0;

        while (!queue.isEmpty()) {
            Integer nextJolt = queue.poll();
            Integer diff = nextJolt - curJolt;

            Integer updatedCount = diffs.getOrDefault(diff, 0) + 1;
            diffs.put(diff, updatedCount);

            curJolt = nextJolt;
        }

        // Last connector has an additional difference of three
        diffs.put(3, diffs.getOrDefault(3, 0) + 1);

        return diffs.get(1) * diffs.get(3);
    }

    public static void main(String[] args) throws IOException {
        List<Integer> vals = new ArrayList<>();
        try (Stream<String> stream = Files.lines(Paths.get("input.txt"))) {
            stream.forEach(line -> {
                vals.add(Integer.parseInt(line));
            });
        }
        System.out.printf("Part 1: %d\n", calculateJoltJumps(vals));
    }
}