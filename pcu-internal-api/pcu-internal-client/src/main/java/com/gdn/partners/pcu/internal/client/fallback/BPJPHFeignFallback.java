package com.gdn.partners.pcu.internal.client.fallback;

import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.feign.BPJPHFeign;
import com.gdn.partners.pcu.internal.client.model.response.BPJPHListResponse;
import com.gdn.partners.pcu.internal.client.model.response.HalalCertificationDetailResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;

@Component
public class BPJPHFeignFallback implements BPJPHFeign {

  @Override
  public BPJPHListResponse<HalalCertificationDetailResponse> getHalalCertificationDetails(String apiKey, int page, int size,
      String no_sertifikat) {
    return new BPJPHListResponse<>(Constants.ERROR_CODE, ErrorMessages.BPJPH_RESPONSE_FAILED, true, null);
  }
}
