package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude()
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSkuAndIndicativePriceResponse extends BaseResponse {
  private Map<String, List<IndicativePriceDetails>> itemSkuToIndicativePriceDetailsMap =
    new HashMap<>();
}
