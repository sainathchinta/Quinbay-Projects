package com.gdn.partners.pcu.internal.client.feign;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.client.factory.ProductWorkflowFeignFallbackFactory;

/**
 * Created by govind on 11/01/2019 AD.
 */

@FeignClient(name = "productWorkflowFeign", url = "${service.pbp.endpoint}", fallbackFactory = ProductWorkflowFeignFallbackFactory.class)
public interface ProductWorkflowFeign {

  @RequestMapping(value = "/api/product-workflow/return-for-correction",
                  method = RequestMethod.POST)
  GdnBaseRestResponse returnForCorrection(@RequestBody ProductReturnForCorrectionRequest request);
}
