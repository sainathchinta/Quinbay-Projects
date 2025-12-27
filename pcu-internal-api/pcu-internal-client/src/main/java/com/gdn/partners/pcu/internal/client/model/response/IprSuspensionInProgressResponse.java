package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class IprSuspensionInProgressResponse extends BaseResponse {
  private static final long serialVersionUID = -703645022371605651L;
  private String productCode;
  private String productSku;
  private String productName;
  private String brand;
  private String brandCode;
  private String categoryCode;
  private String categoryName;
  private Date evidenceRequestedDate;
  private String evidenceRequestedBy;
  private String state;
  private List<String> evidenceUrl = new ArrayList<>();
  private List<String> evidenceFilePath = new ArrayList<>();
  private String evidenceSubmittedNotes;
  private String evidenceRequestedNotes;
  private String evidenceRequestedReasons;
  private String pdpRedirectionUrl;
  private String productMainImage;
  private boolean activeImage = true;
}