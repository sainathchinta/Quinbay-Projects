package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductHistoryWebResponse {

  private String productId;
  private Integer state;
  private String description;
  private List<String> notes;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private List<ProductUpdateHistoryWebResponse> productUpdateHistoryWebResponseList;
}
