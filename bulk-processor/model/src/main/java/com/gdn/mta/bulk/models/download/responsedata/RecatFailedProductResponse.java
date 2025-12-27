package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.models.RecatFailedProducts;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class RecatFailedProductResponse extends BulkDataResponse implements Serializable {

  private static final long serialVersionUID = -2734202387807822736L;
  List<RecatFailedProducts> responseList;

  @Override
  public String toString() {
    return "RecatFailedProductResponse{" + "responseList=" + responseList + '}';
  }
}
