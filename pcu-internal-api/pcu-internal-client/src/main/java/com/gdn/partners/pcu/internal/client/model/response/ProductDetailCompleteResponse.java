package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gda.mta.product.dto.response.PreOrderResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
@EqualsAndHashCode(callSuper = true)
public class ProductDetailCompleteResponse extends ProductDetailResponse {
  private static final long serialVersionUID = 831792072215656337L;

  private PreOrderResponse preOrder;
  private List<RestrictedKeywordsByFieldResponse> restrictedKeywordsDetected;
  private String sizeChartCode;
  private String sizeChartBusinessPartnerCode;
  private String sizeChartName;
  private AiGeneratedFieldsResponse aiGeneratedFieldsResponse;
}
