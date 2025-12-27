package com.gdn.x.mta.distributiontask.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductRetryStatusUpdate {

  private String state;
  private Boolean revised;
  private Boolean edited;
  private String reviewType;
  private Boolean markForDelete;
}
