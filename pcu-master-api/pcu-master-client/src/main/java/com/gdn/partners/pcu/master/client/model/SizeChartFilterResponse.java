package com.gdn.partners.pcu.master.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class SizeChartFilterResponse extends BaseDTOResponse {
  private String sizeChartName;
  private String sizeChartCode;
  private String sizeAttributeName;
  private String sizeAttributeCode;
  private String brandName;
  private String brandCode;
  private String businessPartnerCode;
  private boolean waitingDeletion;
  private byte[] descriptionEnglish;
  private byte[] description;
}