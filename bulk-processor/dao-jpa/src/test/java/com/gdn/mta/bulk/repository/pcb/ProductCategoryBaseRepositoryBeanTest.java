package com.gdn.mta.bulk.repository.pcb;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

public class ProductCategoryBaseRepositoryBeanTest {

  private final static String FOO = "FOO";
  private final static String BAR = "BAR";
  private final static String REQUEST_ID = "REQUEST-ID";
  private final static String USERNAME = "USERNAME";

  @InjectMocks
  private ProductCategoryBaseRepositoryBean productCategoryBaseRepositoryBean;

  @Mock
  private PCBFeign pcbFeign;

  @Captor
  private ArgumentCaptor<CategoryCodeRequest> categoryCodeRequestArgumentCaptor;

  @Test
  public void getAllChildCategoriesFromC1CategoryCodeTest() throws Exception {
    List<String> categories = new ArrayList<>();
    categories.add(FOO);
    categories.add(FOO);
    categories.add(FOO);
    List<String> expectedCategories = new ArrayList<>();
    expectedCategories.add(BAR);
    expectedCategories.add(BAR);
    expectedCategories.add(BAR);
    CategoryCodeResponse response = new CategoryCodeResponse(expectedCategories);
    doReturn(new GdnRestSingleResponse<>(response, REQUEST_ID)).when(pcbFeign)
        .getAllChildCategoriesFromC1CategoryCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
            Mockito.any(CategoryCodeRequest.class));
    List<String> actualCategories = productCategoryBaseRepositoryBean
        .getAllChildCategoriesFromC1CategoryCode(REQUEST_ID, USERNAME, categories);
    verify(pcbFeign).getAllChildCategoriesFromC1CategoryCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
        categoryCodeRequestArgumentCaptor.capture());
    Assertions.assertEquals(categories.size(),
        categoryCodeRequestArgumentCaptor.getValue().getCategoryCodes().size());
    Assertions.assertEquals(categories, categoryCodeRequestArgumentCaptor.getValue().getCategoryCodes());

    Assertions.assertEquals(expectedCategories.size(), actualCategories.size());
    Assertions.assertTrue(actualCategories.containsAll(
        Arrays.asList(expectedCategories.get(0), expectedCategories.get(1),
            expectedCategories.get(2))));
  }

  @Test
  public void getAllChildCategoriesFromC1CategoryCode_nullResponse_test() throws Exception {
    List<String> categories = new ArrayList<>();
    categories.add(FOO);
    categories.add(FOO);
    categories.add(FOO);
    List<String> actualCategories = productCategoryBaseRepositoryBean
        .getAllChildCategoriesFromC1CategoryCode(REQUEST_ID, USERNAME, categories);
    verify(pcbFeign).getAllChildCategoriesFromC1CategoryCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
        categoryCodeRequestArgumentCaptor.capture());
    Assertions.assertEquals(categories.size(),
        categoryCodeRequestArgumentCaptor.getValue().getCategoryCodes().size());
    Assertions.assertTrue(
        categoryCodeRequestArgumentCaptor.getValue().getCategoryCodes().containsAll(categories));
    Assertions.assertTrue(CollectionUtils.isEmpty(actualCategories));
  }

  @Test
  public void getAllChildCategoriesFromC1CategoryCode_nullValueOnResponse_test() throws Exception {
    List<String> categories = new ArrayList<>();
    categories.add(FOO);
    categories.add(FOO);
    categories.add(FOO);
    List<String> expectedCategories = new ArrayList<>();
    CategoryCodeResponse response = new CategoryCodeResponse(expectedCategories);
    doReturn(new GdnRestSingleResponse<>(response, REQUEST_ID)).when(pcbFeign)
        .getAllChildCategoriesFromC1CategoryCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
            Mockito.any(CategoryCodeRequest.class));
    List<String> actualCategories = productCategoryBaseRepositoryBean
        .getAllChildCategoriesFromC1CategoryCode(REQUEST_ID, USERNAME, categories);
    verify(pcbFeign).getAllChildCategoriesFromC1CategoryCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
        categoryCodeRequestArgumentCaptor.capture());
    Assertions.assertEquals(categories.size(), categoryCodeRequestArgumentCaptor.getValue().getCategoryCodes().size());
    Assertions.assertTrue(
        categoryCodeRequestArgumentCaptor.getValue().getCategoryCodes().containsAll(categories));

    Assertions.assertEquals(expectedCategories.size(), actualCategories.size());
    Assertions.assertTrue(actualCategories.isEmpty());
  }

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(pcbFeign);
  }
}
