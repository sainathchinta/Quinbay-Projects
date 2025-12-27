package com.gdn.x.mta.distributiontask.model.dto;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class Pair<K, V> implements Serializable {
  private K key;
  private V value;
}
