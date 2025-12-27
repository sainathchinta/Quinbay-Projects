package com.gdn.mta.product.repository;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.partners.pbp.outbound.margin.feign.MarginFeign;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class MarginRepositoryBeanTest {
  private static final String STORE_ID = "storeId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "USERNAME";
  private static final Double VALUE = 1.0;


  private MarginCategoryResponse marginCategoryResponse;

  @Mock
  MarginFeign marginFeign;

  @InjectMocks
  MarginRepositoryBean marginRepositoryBean;

  @BeforeEach
  public void setUp(){
    MockitoAnnotations.initMocks(this);

    MDC.put(REQUEST_ID , REQUEST_ID);
    MDC.put(USERNAME , USERNAME);
    marginCategoryResponse = new MarginCategoryResponse(REQUEST_ID ,
        STORE_ID , null , null , null , CATEGORY_CODE , null ,
        VALUE , null ,null ,null ,null ,null);
  }

  @AfterEach
  public void tearDown(){
    verifyNoMoreInteractions(this.marginFeign);
    MDC.clear();
  }

  @Test
  public void getMarginForStoreIdAndCategoryCodeTest() throws Exception {
    GdnRestSingleResponse<MarginCategoryResponse> response = new GdnRestSingleResponse<>(
        marginCategoryResponse , REQUEST_ID) ;
    when(this.marginFeign.filterMarginCategoryByCategoryIdAndOrderDate(anyString() ,anyString() ,anyString() ,
        anyString() , anyString() , any())) .thenReturn(response);

    MarginCategoryResponse marginCategoryResponse = this.marginRepositoryBean
        .getMarginForStoreIdAndCategoryCode(STORE_ID , CATEGORY_CODE);

    verify(this.marginFeign).filterMarginCategoryByCategoryIdAndOrderDate(anyString() ,anyString() ,anyString() ,
        anyString() ,anyString()  ,any());
  }

  @Test
  public void getMarginForStoreIdAndCategoryCodeTest_NullResponse() throws Exception {
    when(this.marginFeign.filterMarginCategoryByCategoryIdAndOrderDate(anyString() ,anyString() ,anyString() ,
        anyString() , anyString() , any())) .thenReturn(null);
    try{
      this.marginRepositoryBean
          .getMarginForStoreIdAndCategoryCode(STORE_ID , CATEGORY_CODE);
    }catch (Exception ex){}
    finally {
      verify(this.marginFeign).filterMarginCategoryByCategoryIdAndOrderDate(anyString()  ,anyString() ,anyString() ,
          anyString() , anyString()  ,any());
    }
  }

  @Test
  public void getMarginForStoreIdAndCategoryCodeTest_WhenException() throws Exception {
    when(this.marginFeign.filterMarginCategoryByCategoryIdAndOrderDate(anyString() ,anyString() ,anyString() ,
        anyString() , anyString() , any())) .thenThrow(RuntimeException.class);
    try{
      this.marginRepositoryBean
          .getMarginForStoreIdAndCategoryCode(STORE_ID , CATEGORY_CODE);
    }catch (Exception ex){}
    finally {
      verify(this.marginFeign).filterMarginCategoryByCategoryIdAndOrderDate(anyString()  ,anyString() ,anyString() ,
          anyString() , anyString()  ,any());
    }
  }

  @Test
  public void getMarginForStoreIdAndCategoryCodeTest_WhenResponseFailed() throws Exception {
    GdnRestSingleResponse<MarginCategoryResponse> response = new GdnRestSingleResponse<>(
        marginCategoryResponse , REQUEST_ID) ;
    response.setSuccess(false);
    when(this.marginFeign.filterMarginCategoryByCategoryIdAndOrderDate(anyString() ,anyString() ,anyString() ,
        anyString() , anyString() , any())) .thenReturn(response);
    try{
      this.marginRepositoryBean
          .getMarginForStoreIdAndCategoryCode(STORE_ID , CATEGORY_CODE);
    }catch (Exception ex){}
    finally {
      verify(this.marginFeign).filterMarginCategoryByCategoryIdAndOrderDate(anyString()  ,anyString() ,anyString() ,
          anyString() , anyString()  ,any());
    }
  }

  @Test
  public void getMarginForStoreIdAndCategoryCodeTest_WithDefaultParams() throws Exception {
    MDC.remove(REQUEST_ID);
    MDC.remove(USERNAME);
    GdnRestSingleResponse<MarginCategoryResponse> response = new GdnRestSingleResponse<>(
        marginCategoryResponse , REQUEST_ID) ;
    when(this.marginFeign.filterMarginCategoryByCategoryIdAndOrderDate(anyString() ,anyString() ,anyString() ,
        anyString() , anyString() , any())) .thenReturn(response);

    MarginCategoryResponse marginCategoryResponse = this.marginRepositoryBean
        .getMarginForStoreIdAndCategoryCode(STORE_ID , CATEGORY_CODE);

    verify(this.marginFeign).filterMarginCategoryByCategoryIdAndOrderDate(anyString()  ,anyString() ,anyString() ,
        anyString(),anyString() ,any());
  }
}