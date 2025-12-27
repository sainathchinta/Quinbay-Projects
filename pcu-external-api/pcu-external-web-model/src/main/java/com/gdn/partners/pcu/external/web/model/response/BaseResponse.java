package com.gdn.partners.pcu.external.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

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
public class BaseResponse {
  private static final long serialVersionUID = -255944134096957061L;

  private String id;
  private String storeId;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private Long version;
}
