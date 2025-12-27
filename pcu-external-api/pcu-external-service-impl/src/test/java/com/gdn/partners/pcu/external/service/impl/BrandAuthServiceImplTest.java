package com.gdn.partners.pcu.external.service.impl;


import com.gdn.partners.pcu.external.web.model.request.BulkBrandData;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandDataRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.mockito.Mockito.when;

@ExtendWith(SpringExtension.class)
public class BrandAuthServiceImplTest {

  @InjectMocks
  private BrandAuthServiceImpl brandAuthService;

  @Mock
  private HttpServletResponse httpServletResponse;

  @Mock
  private ServletOutputStream servletOutputStream;


  private List<BulkBrandData> bulkBrandData = new ArrayList<>();
  private BulkBrandDataRequest bulkBrandDataRequest = new BulkBrandDataRequest();
  private static final String FILE_BULK_SELECTED_BRAND_DOWNLOAD = "bulk-selected-brand-template";
  private static final String REQUEST_ID = "requestId";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String SELLER_CODE = "sellerCode";
  private static final String SELLER_NAME = "sellerName";
  private static final Long BRAND_AUTH_START_DATE = 202314L;
  private static final Long BRAND_AUTH_END_DATE = 202314L;

  @BeforeEach
  public void setup() throws IOException {
    MockitoAnnotations.openMocks(this);
    bulkBrandData = Arrays.asList(
        new BulkBrandData(BRAND_CODE, BRAND_NAME, SELLER_CODE, SELLER_NAME, BRAND_AUTH_START_DATE,
            BRAND_AUTH_END_DATE));
    bulkBrandDataRequest.setDownloadRequest(bulkBrandData);
    when(httpServletResponse.getOutputStream()).thenReturn(servletOutputStream);
  }

  @Test
  public void selectedBrandBulkDownloadTest() throws Exception {
    brandAuthService.selectedBrandBulkDownload(httpServletResponse, bulkBrandDataRequest);
  }
}