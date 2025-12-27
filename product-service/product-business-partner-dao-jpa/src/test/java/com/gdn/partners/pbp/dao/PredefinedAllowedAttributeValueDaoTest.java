package com.gdn.partners.pbp.dao;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

public class PredefinedAllowedAttributeValueDaoTest {

  private static final String REQUEST_ID = "reqId";

  private static final String VALUE = "value";

  private static final String USERNAME = "username";

  private static final String ATTRIBUTECODE = "attributeCode";

  private static final String ATTRIBUTEID = "attributeId";

  private static final String STOREID = "10001";

  @InjectMocks
  private PredefinedAllowedAttributeValueDao instance;

  @Mock
  private PCBFeign pcbFeign;

  private GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> response;

  @BeforeEach
  public void setUp() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    MockitoAnnotations.initMocks(this);
    response =
        new GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse>(
            new PredefinedAllowedAttributeValueResponse(VALUE, 0, STOREID), REQUEST_ID);
  }

  @Test
  public void testFindByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse() throws Exception {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> responseList =
        new GdnRestListResponse<PredefinedAllowedAttributeValueResponse>(
            new ArrayList<PredefinedAllowedAttributeValueResponse>(Arrays.asList(response
                .getValue())), new PageMetaData(10, 0, 1), REQUEST_ID);
    Mockito.when(this.pcbFeign.getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyInt(), Mockito.anyString(), Mockito.anyString())).thenReturn(responseList);
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> responseGdnRestListResponse =
        instance.findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(REQUEST_ID, new GdnRestListRequest(0, 10),
            ATTRIBUTEID, VALUE);
    Mockito.verify(this.pcbFeign)
        .getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(),
            Mockito.anyString(), Mockito.anyString());
    Assertions.assertEquals(VALUE, responseGdnRestListResponse.getContent().get(0).getValue());
  }

  @Test
  public void testFindByStoreIdAndMatchAttributeCodeAndValue() throws Exception {
    Mockito.when(this.pcbFeign.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
            GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), REQUEST_ID, USERNAME, ATTRIBUTECODE, VALUE, false))
        .thenReturn(this.response);
    GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> responseGdnRestSingleResponse =
        instance.findByStoreIdAndMatchAttributeCodeAndValue(REQUEST_ID, ATTRIBUTECODE, VALUE);
    Mockito.verify(this.pcbFeign)
        .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), REQUEST_ID,
            USERNAME, ATTRIBUTECODE, VALUE, false);
    Assertions.assertEquals(VALUE, responseGdnRestSingleResponse.getValue().getValue());

  }

}
