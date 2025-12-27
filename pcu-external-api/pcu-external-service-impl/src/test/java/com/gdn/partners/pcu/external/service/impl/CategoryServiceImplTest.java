package com.gdn.partners.pcu.external.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.web.model.response.CategoryWebResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * Created by govind on 19/12/2018 AD.
 */
public class CategoryServiceImplTest {

  private static final String REQUEST_ID = "attrequestribute-id";
  private static final String CATEGORY_CODE = "category-code";
  private static final String CATEGORY_ID = "category-id";
  private static final String ATTRIBUTE_ID = "attribute-id";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_BY = "updatedBy";

  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private CategoryServiceImpl categoryService;


  private GdnRestListResponse<CategoryResponse> gdnRestListResponse;
  private GdnRestSingleResponse<CategoryDetailResponse> categoryDetailResponseGdnRestSingleResponse;
  private GdnRestSingleResponse<AttributeResponse> attributeResponseGdnRestSingleResponse;

  private CategoryResponse categoryResponse;
  private CategoryDetailResponse categoryDetailResponse;
  private AttributeResponse attributeResponse;
  private List<String> categoryCodes;
  private List<String> categoryCodes1;
  private CategoryMultipleIdRequest bulkRequest;
  private CategoryMultipleIdRequest bulkRequest1;
  private CategoryMultipleIdRequest bulkRequest2;
  private CategoryDTO categoryDTO = new CategoryDTO();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    gdnRestListResponse = new GdnRestListResponse<CategoryResponse>();
    gdnRestListResponse.setSuccess(Boolean.TRUE);
    List<CategoryResponse> categoryResponseList = new ArrayList<>();
    categoryResponse = new CategoryResponse();
    categoryResponseList.add(categoryResponse);
    categoryResponse.setInternalActivationInterval(24);
    gdnRestListResponse.setContent(categoryResponseList);
    ReflectionTestUtils.setField(categoryService, "internalActivationPeriod", 72);
    ReflectionTestUtils.setField(categoryService, "categoryBatchSize", 30);

    categoryDetailResponse = new CategoryDetailResponse();
    attributeResponse = new AttributeResponse();
    categoryDetailResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    attributeResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, attributeResponse, REQUEST_ID);

    categoryCodes = new ArrayList<>();
    categoryCodes.add(CATEGORY_CODE);

    bulkRequest = new CategoryMultipleIdRequest();
    bulkRequest.setCategoryCode(categoryCodes);

    categoryCodes1 = new ArrayList<>();
    for (int i = 0; i < 35; i++) {
      categoryCodes1.add(CATEGORY_CODE);
    }

    bulkRequest1 = new CategoryMultipleIdRequest();
    bulkRequest1.setCategoryCode(categoryCodes1.subList(0, 30));

    bulkRequest2 = new CategoryMultipleIdRequest();
    bulkRequest2.setCategoryCode(categoryCodes1.subList(30, 35));

    categoryDTO.setUpdatedBy(UPDATED_BY);
    categoryDTO.setCreatedBy(CREATED_BY);
    categoryDTO.setUpdatedDate(new Date());
    categoryDTO.setCreatedDate(new Date());
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(pcbFeign);
  }


  @Test
  public void findInternalActivationIntervalInDaysByCategoryCodeTest() throws Exception {
    Mockito.when(pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE)).thenReturn(
        gdnRestListResponse);
    String value = categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
    Assertions.assertNotNull(value);
    Assertions.assertEquals("1", value);
  }

  @Test
  public void findInternalActivationIntervalInDaysByCategoryCodeTest_withDefault() throws Exception {
    categoryResponse.setInternalActivationInterval(null);
    Mockito.when(pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE)).thenReturn(
        gdnRestListResponse);
    String value = categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(Mockito.eq(CATEGORY_CODE));
    Assertions.assertNotNull(value);
    Assertions.assertEquals("3", value);
  }

  @Test
  public void findInternalActivationIntervalInDaysByCategoryCode_whenResponseNullTest()
      throws Exception {
    Mockito.when(pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE)).thenReturn(null);
    try {
      categoryService.findInternalActivationIntervalInDaysByCategoryCode(CATEGORY_CODE);
    } catch (ClientException ex) {
      Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(Mockito.eq(CATEGORY_CODE));
    }
  }

  @Test
  public void getCategoryDetailTest(){
    Mockito.when(pcbFeign.getCategoryDetail(CATEGORY_ID)).thenReturn(categoryDetailResponseGdnRestSingleResponse);
    CategoryDetailResponse categoryDetailResponse = categoryService.getCategoryDetail(CATEGORY_ID);
    Mockito.verify(pcbFeign).getCategoryDetail(CATEGORY_ID);
    Assertions.assertNotNull(categoryDetailResponse);
  }

  @Test
  public void getCategoryDetailExceptionTest() {
    Mockito.when(pcbFeign.getCategoryDetail(CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      categoryService.getCategoryDetail(CATEGORY_ID);
    } catch (ClientException ex) {
      Mockito.verify(pcbFeign).getCategoryDetail(Mockito.eq(CATEGORY_ID));
    }
  }

  @Test
  public void getAttributeDetailTest(){
    Mockito.when(pcbFeign.getAttributeDetail(ATTRIBUTE_ID, false)).thenReturn(attributeResponseGdnRestSingleResponse);
    AttributeResponse attributeResponse = categoryService.getAttributeDetail(ATTRIBUTE_ID);
    Mockito.verify(pcbFeign).getAttributeDetail(ATTRIBUTE_ID, false);
    Assertions.assertNotNull(attributeResponse);
  }

  @Test
  public void getAttributeDetailExceptionTest() {
    Mockito.when(pcbFeign.getAttributeDetail(ATTRIBUTE_ID, false))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      categoryService.getAttributeDetail(ATTRIBUTE_ID);
    } catch (ClientException ex) {
      Mockito.verify(pcbFeign).getAttributeDetail(Mockito.eq(ATTRIBUTE_ID), Mockito.eq(false));
    }
  }

  @Test
  public void getCategoriesByCategoryCodesTest() throws Exception {
    Mockito.when(pcbFeign.getCategoriesByCategoryCodes(0, Integer.MAX_VALUE, Boolean.TRUE.toString(),
        new ObjectMapper().writeValueAsString(bulkRequest))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(new CategoryDTO()), new PageMetaData(), REQUEST_ID));
    List<CategoryWebResponse> response = categoryService.getCategoriesByCategoryCodes(categoryCodes);
    Mockito.verify(pcbFeign).getCategoriesByCategoryCodes(0, Integer.MAX_VALUE, Boolean.TRUE.toString(),
        new ObjectMapper().writeValueAsString(bulkRequest));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.size());
  }

  @Test
  public void getCategoriesByCategoryCodesTest_withBatching() throws Exception {
    Mockito.when(pcbFeign.getCategoriesByCategoryCodes(0, Integer.MAX_VALUE, Boolean.TRUE.toString(),
        new ObjectMapper().writeValueAsString(bulkRequest1))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(new CategoryDTO()), new PageMetaData(), REQUEST_ID));
    Mockito.when(pcbFeign.getCategoriesByCategoryCodes(0, Integer.MAX_VALUE, Boolean.TRUE.toString(),
        new ObjectMapper().writeValueAsString(bulkRequest2))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(new CategoryDTO()), new PageMetaData(), REQUEST_ID));
    List<CategoryWebResponse> response = categoryService.getCategoriesByCategoryCodes(categoryCodes1);
    Mockito.verify(pcbFeign).getCategoriesByCategoryCodes(0, Integer.MAX_VALUE, Boolean.TRUE.toString(),
        new ObjectMapper().writeValueAsString(bulkRequest1));
    Mockito.verify(pcbFeign).getCategoriesByCategoryCodes(0, Integer.MAX_VALUE, Boolean.TRUE.toString(),
        new ObjectMapper().writeValueAsString(bulkRequest2));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(2, response.size());
  }

  @Test
  public void getCategoriesByCategoryCodesTest_clearConfidentialInformation() throws Exception {
    Mockito.when(pcbFeign.getCategoriesByCategoryCodes(0, Integer.MAX_VALUE, Boolean.TRUE.toString(),
        new ObjectMapper().writeValueAsString(bulkRequest))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(categoryDTO), new PageMetaData(), REQUEST_ID));
    List<CategoryWebResponse> response = categoryService.getCategoriesByCategoryCodes(categoryCodes);
    Mockito.verify(pcbFeign).getCategoriesByCategoryCodes(0, Integer.MAX_VALUE, Boolean.TRUE.toString(),
        new ObjectMapper().writeValueAsString(bulkRequest));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.size());
    Assertions.assertTrue(StringUtils.isEmpty(response.get(0).getUpdatedBy()));
    Assertions.assertTrue(StringUtils.isEmpty(response.get(0).getCreatedBy()));
    Assertions.assertNull(response.get(0).getUpdatedDate());
    Assertions.assertNull(response.get(0).getCreatedDate());
  }
}
