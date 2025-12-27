package com.gdn.partners.pbp.distributiontask;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pbp.dao.IPredefinedAllowedAttributeValueDao;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

public class PredefinedAttributeAllowedValueServiceBeanTest {

  private static final String REQUEST_ID = "reqId";

  private static final String VALUE = "value";

  private static final String USERNAME = "username";

  private static final String ATTRIBUTECODE = "attributeCode";

  private static final String ATTRIBUTEID = "attributeId";

  private static final String STOREID = "10001";

  @InjectMocks
  private PredefinedAttributeAllowedValueServiceBean instance;

  @Mock
  private IPredefinedAllowedAttributeValueDao predefinedAllowedAtributeValueDaoService;

  private GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> response;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.response =
        new GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse>(
            new PredefinedAllowedAttributeValueResponse(VALUE, 0, STOREID), REQUEST_ID);
  }

  @Test
  public void testFindByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse() throws Exception {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> responseList =
        new GdnRestListResponse<PredefinedAllowedAttributeValueResponse>(
            new ArrayList<PredefinedAllowedAttributeValueResponse>(Arrays.asList(response
                .getValue())), new PageMetaData(10, 0, 1), REQUEST_ID);
    Mockito
        .when(
            this.predefinedAllowedAtributeValueDaoService
                .findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(Mockito.anyString(),
                    Mockito.any(), Mockito.anyString(),
                    Mockito.anyString())).thenReturn(responseList);
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response = this.instance
        .findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(REQUEST_ID, new GdnRestListRequest(0, 10), ATTRIBUTEID, VALUE);
    Mockito.verify(this.predefinedAllowedAtributeValueDaoService)
        .findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.any(), Mockito.anyString(), Mockito.anyString());
    Assertions.assertEquals(VALUE, response.getContent().get(0).getValue());
  }

  @Test
  public void testFindByStoreIdAndMatchAttributeCodeAndValue() throws Exception {
    Mockito.when(
        this.predefinedAllowedAtributeValueDaoService.findByStoreIdAndMatchAttributeCodeAndValue(
            REQUEST_ID, ATTRIBUTECODE, VALUE)).thenReturn(response);
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        this.instance.findByStoreIdAndMatchAttributeCodeAndValue(REQUEST_ID, ATTRIBUTECODE, VALUE);
    Mockito.verify(this.predefinedAllowedAtributeValueDaoService)
        .findByStoreIdAndMatchAttributeCodeAndValue(REQUEST_ID, ATTRIBUTECODE, VALUE);
    Assertions.assertEquals(VALUE, predefinedAllowedAttributeValueResponse.getValue());
  }
  
  @Test
  public void testFindByStoreIdAndMatchAttributeCodeAndValueNotOk() throws Exception {
    this.response.setSuccess(false);
    Mockito.when(
        this.predefinedAllowedAtributeValueDaoService.findByStoreIdAndMatchAttributeCodeAndValue(
            REQUEST_ID, ATTRIBUTECODE, VALUE)).thenReturn(response);
    try{
      Assertions.assertThrows(Exception.class, () -> {
        this.instance.findByStoreIdAndMatchAttributeCodeAndValue(REQUEST_ID, ATTRIBUTECODE, VALUE);
      });
    }catch(Exception e){
      Mockito.verify(this.predefinedAllowedAtributeValueDaoService)
      .findByStoreIdAndMatchAttributeCodeAndValue(REQUEST_ID, ATTRIBUTECODE, VALUE); 
      throw e;
    }
  }
}
