package com.gdn.mta.bulk.helper;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;

public class POIUtilTest {

  @InjectMocks
  private POIUtil poiUtil;

  @AfterEach
  public void finalizeTest() {
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  void testCreateUnmappedSkusSheetWithData() throws Exception {
    String username = "testUser";
    String requestId = "req123";
    String categoryName = "Electronics";
    int maxCatCount = 2;
    Map<String, List<String>> categoryNameHierarchy = new HashMap<>();
    categoryNameHierarchy.put("CAT001", Arrays.asList("Category1", "SubCategory1"));
    categoryNameHierarchy.put("CAT002", Arrays.asList("Category2", "SubCategory2"));
    List<UnmappedSkuResponse> unmappedSkuResponses = new ArrayList<>();
    UnmappedSkuResponse response1 = new UnmappedSkuResponse("CAT001", "PROD001", "Product One", "CAT001", new Date());
    UnmappedSkuResponse response2 = new UnmappedSkuResponse("CAT002", "PROD002", "Product Two", "CAT002", new Date());
    unmappedSkuResponses.add(response1);
    unmappedSkuResponses.add(response2);
    Workbook workbook =
        poiUtil.createUnmappedSkusSheetWithData(username, requestId, categoryName, maxCatCount, categoryNameHierarchy,
            unmappedSkuResponses);
    Assertions.assertNotNull(workbook);
  }
}
