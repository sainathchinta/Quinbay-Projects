package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkConfigSummaryResponse extends BulkDataResponse implements Serializable {

  private static final long serialVersionUID = -3979626141260816789L;

  List<BulkConfigDataResponse> bulkConfigDataResponseList;

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("bulkConfigDataResponseList", bulkConfigDataResponseList).toString();
  }

}

