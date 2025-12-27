package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.response.ReviewProductResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 29/01/2019 AD.
 */

@Data
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkReviewProductResponse extends BulkDataResponse implements Serializable {
  private static final long serialVersionUID = -4189645376666866759L;


  List<ReviewProductResponse> responseList;

  public BulkReviewProductResponse(List<ReviewProductResponse> responseList) {
    this.responseList = responseList;
  }

  public List<ReviewProductResponse> getResponseList() {
    return responseList;
  }

  public void setResponseList(List<ReviewProductResponse> responseList) {
    this.responseList = responseList;
  }
}
