package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class CategoryHistoryServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String CATEGORY_CODE = "CAT-60027";

  @InjectMocks
  private CategoryHistoryServiceImpl categoryHistoryService;

  @Mock
  private PCBFeign pcbFeign;

  private CategoryHistoryResponse categoryHistoryResponse;

  private GdnRestListResponse<CategoryHistoryResponse> gdnRestListResponse;

  @BeforeEach
  public void setup() {
    gdnRestListResponse = new GdnRestListResponse<CategoryHistoryResponse>();
    gdnRestListResponse.setSuccess(Boolean.TRUE);
    List<CategoryHistoryResponse> categoryResponseList = new ArrayList<>();
    categoryHistoryResponse = new CategoryHistoryResponse();
    categoryHistoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponseList.add(categoryHistoryResponse);
    gdnRestListResponse.setContent(categoryResponseList);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  void fetchCategoryHistoryTest() {
    Mockito.when(pcbFeign.fetchCategoryHistory(CATEGORY_CODE, 0, 20))
        .thenReturn(gdnRestListResponse);
    categoryHistoryService.categoryHistory(STORE_ID, CATEGORY_CODE, 0, 20);
    Mockito.verify(pcbFeign).fetchCategoryHistory(CATEGORY_CODE, 0, 20);
  }
}
