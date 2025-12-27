package com.gdn.partners.pbp.outbound.xProduct.feign;

import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PromoEligibilityRequest extends BaseRequest implements Serializable {
  private static final long serialVersionUID = -2216231155272907613L;
  private Map<String, Set<String>> businessPartnerAndProductSkuList = new HashMap<>();
}
