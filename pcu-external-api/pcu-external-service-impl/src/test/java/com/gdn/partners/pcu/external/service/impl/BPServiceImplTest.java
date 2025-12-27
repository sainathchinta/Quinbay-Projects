package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.BPServiceFeign;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.x.bpservice.dto.request.BusinessPartnerHistoryRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * Created by govind on 20/12/2018 AD.
 */
public class BPServiceImplTest {

  @Mock
  private BPServiceFeign bpServiceFeign;

  @InjectMocks
  private BPServiceImpl bpService;


  private GdnBaseRestResponse gdnBaseRestResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(Boolean.TRUE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(bpServiceFeign);
  }

  @Test
  public void saveBusinessPartnerHistoryTest() throws Exception{
    BusinessPartnerHistoryRequest request = new BusinessPartnerHistoryRequest();
    Mockito.when(bpServiceFeign.saveBusinessPartnerHistory(request)).thenReturn(gdnBaseRestResponse);
    bpService.saveBusinessPartnerHistory(request);
    Mockito.verify(bpServiceFeign).saveBusinessPartnerHistory(request);
  }

  @Test
  public void saveBusinessPartnerHistoryClientExceptionTest() throws Exception {
    BusinessPartnerHistoryRequest request = new BusinessPartnerHistoryRequest();
    Mockito.when(bpServiceFeign.saveBusinessPartnerHistory(request)).thenReturn(null);
    try {
      bpService.saveBusinessPartnerHistory(request);
    } catch (ClientException ex) {
      Mockito.verify(bpServiceFeign).saveBusinessPartnerHistory(request);
    }
  }

}
