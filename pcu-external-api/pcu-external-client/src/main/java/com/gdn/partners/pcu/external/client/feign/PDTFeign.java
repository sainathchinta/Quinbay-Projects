package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.factory.PDTFeignFallbackFactory;
import com.gdn.partners.pcu.external.client.model.SubmitEvidenceIPRRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "pdtFeign", url = "${service.pdt.endpoint}", fallbackFactory = PDTFeignFallbackFactory.class)
public interface PDTFeign {
  @RequestMapping(value="/api/ipr/submit-evidence",method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse submitEvidence(@RequestParam("requestId") String requestId, @RequestBody
      SubmitEvidenceIPRRequest submitEvidenceIPRRequest);
}