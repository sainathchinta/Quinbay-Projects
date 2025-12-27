package com.gdn.x.productcategorybase.solr.util;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by Kesha on 24/04/16.
 */
public class ListSplitter<V> {

  public List<List<V>> splitByMaxListSize(List<V> originalList, Integer maxLength) {
    List<List<V>> newList = new ArrayList<>();
    int sizeNewList = originalList.size() / maxLength + 1;
    if (sizeNewList <= 1) {
      newList.add(originalList);
    } else {
      for (int i = 0; i < sizeNewList; i++) {
        int startIndex = i * maxLength;
        int endIndex = (i + 1) * maxLength;
        endIndex = endIndex > originalList.size() ? originalList.size() : endIndex;
        newList.add(originalList.subList(startIndex, endIndex));
      }
    }

    return newList;
  }

  public List<List<V>> splitByMaxChunkNumber(List<V> originalList, Integer numOfChunk) {
    List<List<V>> newList = new ArrayList<>();
    int maxLength = originalList.size() / numOfChunk;
    if (numOfChunk <= 1 || originalList.size() < numOfChunk) {
      newList.add(originalList);
    } else {
      int remaining = originalList.size() % numOfChunk;
      int startIndex = 0;
      int endIndex = maxLength;
      for (int i = 0; i < numOfChunk; i++) {
        if(remaining-- > 0){
          endIndex++;
        }
        newList.add(originalList.subList(startIndex, endIndex));
        startIndex = endIndex;
        endIndex = endIndex + maxLength;
      }
    }

    return newList;
  }

}
