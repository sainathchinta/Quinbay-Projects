package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by shivam on 08/07/2019 AD.
 */

@Data
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkVendorSummaryResponse extends BulkDataResponse implements Serializable {

  private static final long serialVersionUID = -8846313879676844922L;
  List<DistributionProductResponse> responseList;

  public BulkVendorSummaryResponse(List<DistributionProductResponse> responseList) {
    this.responseList = responseList;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("responseList", responseList).toString();
  }

}
