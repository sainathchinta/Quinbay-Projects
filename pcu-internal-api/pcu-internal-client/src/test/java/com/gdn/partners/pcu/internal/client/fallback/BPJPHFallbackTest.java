package com.gdn.partners.pcu.internal.client.fallback;



import com.gdn.partners.pcu.internal.client.model.response.BPJPHListResponse;
import com.gdn.partners.pcu.internal.client.model.response.HalalCertificationDetailResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class BPJPHFallbackTest {

  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String CERTIFICATE_NUMBER = "ID00410000005790420";
  private static final int ERROR_CODE = 400;
  private static final String API_KEY = "api-key";

  private BPJPHFeignFallback bpjphFeignFallback = new BPJPHFeignFallback();

  @Test
  public void getHalalCeritificationDetailsTest() {
    BPJPHListResponse<HalalCertificationDetailResponse> halalCertificationDetailResponseBPJPHListResponse =
        bpjphFeignFallback.getHalalCertificationDetails(API_KEY, PAGE, SIZE, CERTIFICATE_NUMBER);
    Assertions.assertEquals(ERROR_CODE, halalCertificationDetailResponseBPJPHListResponse.getStatusCode());
  }
}
