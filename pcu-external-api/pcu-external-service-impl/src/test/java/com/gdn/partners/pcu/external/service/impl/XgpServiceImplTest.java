package com.gdn.partners.pcu.external.service.impl;


import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.XgpFeign;
import com.gdn.partners.pcu.external.service.model.request.FullImageUploadRequest;
import com.gdn.partners.pcu.external.service.model.request.MediumImageUploadRequest;
import com.gdn.partners.pcu.external.service.model.request.ThumbNailImageUploadRequest;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class XgpServiceImplTest {

  private XgpImageScaleRequest xgpImageScaleRequest;
  private static final String CLIENT_HOST = "pcu-external-api";

  @InjectMocks
  private XgpServiceImpl xgpService;

  @Mock
  private XgpFeign xgpFeign;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    xgpImageScaleRequest = new XgpImageScaleRequest();
    xgpImageScaleRequest.setThumbNailImageUploadRequest(new ThumbNailImageUploadRequest());
    xgpImageScaleRequest.setMediumImageUploadRequest(new MediumImageUploadRequest());
    xgpImageScaleRequest.setFullImageUploadRequest(new FullImageUploadRequest());
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(xgpFeign);
  }

  @Test
  public void scaleActiveProductNewImagesTest() {
    Mockito.when(xgpFeign.scaleActiveProductNewImages(CLIENT_HOST, xgpImageScaleRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    xgpService.scaleActiveProductNewImages(xgpImageScaleRequest);
    Mockito.verify(xgpFeign).scaleActiveProductNewImages(CLIENT_HOST, xgpImageScaleRequest);
  }
}