package com.gdn.partners.pcu.master.client.feign;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gda.mta.product.dto.AutoQcConfigRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;

import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.master.client.factory.PBPFeignFallbackFactory;

@FeignClient(name = "pbpFeign", url = "${service.pbp.endpoint}", fallbackFactory = PBPFeignFallbackFactory.class)
public interface PBPFeign {

  @RequestMapping(value = "/api/auto-qc-config/update_rule/{ruleName}", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse update(@RequestParam("isNeedRevisionConfig") boolean isNeedRevisionConfig, @PathVariable("ruleName") String ruleName, @RequestBody AutoQcConfigRequest autoQcConfigRequest);

  @RequestMapping(value = "/api/auto-qc-config/getRules", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<AutoApprovalRulesListResponse> getAutoApprovalRules();
}
