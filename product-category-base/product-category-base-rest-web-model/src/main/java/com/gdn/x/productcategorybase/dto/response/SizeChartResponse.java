package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
public class SizeChartResponse extends BaseDTOResponse {
  public String sizeChartName;
  public String sizeChartCode;
  public String sizeAttributeName;
  public String sizeAttributeCode;
  public String brandName;
  public String brandCode;
  public String businessPartnerCode;
  public boolean waitingDeletion;
  private byte[] descriptionEnglish;
  private byte[] description;
}