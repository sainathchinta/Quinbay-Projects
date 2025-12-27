package com.gdn.partners.pbp.outbound.sap;

import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pbp.outbound.sap.feign.SapFeign;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;

public class SapOutboundBeanTest {

  private static final String CODE = "code";

  @InjectMocks
  private SapOutboundBean sapOutboundBean;

  @Mock
  private SapFeign sapFeign;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(sapFeign);
  }

  @Test
  public void getCogsValueResponse() {
    Mockito.when(sapFeign.getCogsValue(CODE))
        .thenReturn(new GdnRestSimpleResponse<>(null, null, true, null, new CogsValueResponse()));
    sapOutboundBean.getCogsValueResponse(CODE);
    Mockito.verify(sapFeign).getCogsValue(CODE);
  }

  @Test
  public void getCogsValueSuccessFalseResponse() {
    Mockito.when(sapFeign.getCogsValue(CODE))
        .thenReturn(new GdnRestSimpleResponse<>(null, null, false, null, new CogsValueResponse()));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        sapOutboundBean.getCogsValueResponse(CODE);
      });
    } finally {
      Mockito.verify(sapFeign).getCogsValue(CODE);
    }
  }

  @Test
  public void getCogsValueNullResponse() {
    Mockito.when(sapFeign.getCogsValue(CODE)).thenReturn(new GdnRestSimpleResponse<>(null, null, true, null, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        sapOutboundBean.getCogsValueResponse(CODE);
      });
    } finally {
      Mockito.verify(sapFeign).getCogsValue(CODE);
    }
  }
}