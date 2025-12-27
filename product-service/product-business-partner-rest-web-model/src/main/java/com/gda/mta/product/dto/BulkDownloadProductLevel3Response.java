package com.gda.mta.product.dto;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by Vishal on 21/02/17.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDownloadProductLevel3Response extends BaseResponse {
  private static final long serialVersionUID = -8397260765849304189L;
  private List<ProductLevel3SummaryResponse> productLevel3SummaryResponses;
  private Map<String, String> exceptionMap = new HashMap<>();
  private PageMetaData pageMetaData = new PageMetaData();

  public BulkDownloadProductLevel3Response(List<ProductLevel3SummaryResponse> productLevel3SummaryResponses, Map<String, String> exceptionMap){
    this.productLevel3SummaryResponses = productLevel3SummaryResponses;
    this.exceptionMap = exceptionMap;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("productLevel3SummaryResponses", productLevel3SummaryResponses)
        .append("exceptionMap", exceptionMap).append("pageMetaData", pageMetaData).toString();
  }
}
