package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.factory.BPServiceFeignFallbackFactory;
import com.gdn.x.bpservice.dto.request.BusinessPartnerHistoryRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

/**
 * Created by govind on 12/12/2018 AD.
 */
@FeignClient(name = "bpServiceFeign", url = "${service.bpService.endpoint}", fallbackFactory = BPServiceFeignFallbackFactory.class)
public interface BPServiceFeign {

  @RequestMapping(value = "/api/business-partner-history/save", method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse saveBusinessPartnerHistory(@RequestBody BusinessPartnerHistoryRequest businessPartnerHistoryRequest);

}
