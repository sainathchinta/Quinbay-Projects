package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AgpResponseVo {
  private int took;
  private boolean timed_out;
  private Shards _shards;
  private Hits1 hits;
  private Aggregations aggregations;

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class Shards {
    private int total;
    private int successful;
    private int skipped;
    private int failed;
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class Hits1 {
    private int total;
    private double max_score;
    private List<Object> hits;
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class Aggregations {
    @JsonProperty(value = "sterms#value")
    private SkuTerms skuTerms;
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class SkuTerms {
    private int doc_count_error_upper_bound;
    private int sum_other_doc_count;
    private List<Bucket> buckets;
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class Bucket {
    private String key;
    private int doc_count;
  }
}
