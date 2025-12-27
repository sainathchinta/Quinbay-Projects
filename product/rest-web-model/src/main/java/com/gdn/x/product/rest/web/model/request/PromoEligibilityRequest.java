package com.gdn.x.product.rest.web.model.request;

import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PromoEligibilityRequest extends BaseRequest {
  Map<String, Set<String>> businessPartnerAndProductSkuList = new HashMap<>();
}
