package com.gdn.mta.bulk.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@Data
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AgpSimpleQueryResponse {
  private int took;
  private boolean timed_out;
  private ShardsResponse _shards;
  private HitsResponse hits;
}
