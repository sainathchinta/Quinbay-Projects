package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import com.gdn.mta.bulk.models.BrandPredictionRequest;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.fasterxml.jackson.core.type.TypeReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;


public class CategoryAndBrandPredictionServiceBeanTest {
    @InjectMocks
    private CategoryAndBrandPredictionServiceBean categoryAndBrandPredictionServiceBean;

    @Mock
    private BulkProcessDataService bulkProcessDataService;

    @Mock
    private BulkProcessImageQCService bulkProcessImageQCService;

    @Mock
    private ProductCategoryPredictionOutboundService productCategoryPredictionOutboundService;

    @Mock
    private BrandPredictionOutboundService brandPredictionOutboundService;

    @Mock
    private PCBOutboundService pcbOutboundService;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private BulkProcessService bulkProcessService;

    private final String STORE_ID = "10001";
    private final String PARENT_PRODUCT = "parentProduct";
    private final String BULK_PROCESS_CODE = "bulkProcessCode";
    private final String BULK_PROCESS_ID = "bulkProcessId";
    private final String PRODUCT_NAME = "productName";
    private final String BRAND_NAME = "brandName";
    private final String EXTERNAL_CATEGORY = "externalCategory";
    private final String CATEGORY_CODE = "categoryCode";
    private final String TEST_DATA = "Test data";
    private List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    private BulkProcessImageQC bulkProcessImageQC;
    private Map<String, String> DS_RESPONSE = new HashMap<>();
    private List<CategoryHierarchyResponse> categoryHierarchyResponses = new ArrayList<>();
    private BrandAndCategoryPredictionRequest brandAndCategoryPredictionRequest =
        new BrandAndCategoryPredictionRequest();
    private LinkedHashMap<String, Object> rowDataJson = new LinkedHashMap<>();
    private BrandPredictionRequest brandPredictionRequest = new BrandPredictionRequest();
    private BulkProcess bulkProcess;


    @BeforeEach
    public void setup() throws Exception {
        MockitoAnnotations.initMocks(this);

        bulkProcessImageQC = new BulkProcessImageQC();
        bulkProcess = new BulkProcess();
        BulkProcessData bulkProcessData = new BulkProcessData();
        bulkProcessData.setBulkRequestData(TEST_DATA);
        bulkProcessDataList.add(bulkProcessData);
        CategoryResponse categoryResponse = new CategoryResponse();
        CategoryHierarchyResponse categoryHierarchyResponse = new CategoryHierarchyResponse();
        categoryHierarchyResponse.setCategoryHierarchy(Collections.singletonList(categoryResponse));
        categoryHierarchyResponses.add(categoryHierarchyResponse);

        brandAndCategoryPredictionRequest.setExternalCategory(EXTERNAL_CATEGORY);
        brandAndCategoryPredictionRequest.setStoreId(STORE_ID);
        brandAndCategoryPredictionRequest.setParentProduct(PARENT_PRODUCT);
        brandAndCategoryPredictionRequest.setProductName(PRODUCT_NAME);
        brandAndCategoryPredictionRequest.setBulkProcessCode(BULK_PROCESS_CODE);
        brandAndCategoryPredictionRequest.setBulkProcessId(BULK_PROCESS_ID);

        brandPredictionRequest.setProductName(PRODUCT_NAME);
        Mockito.when(
          bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(bulkProcess);
        Mockito.when(bulkProcessDataService
          .findByBulkProcessIdAndParentProductAndStatus(STORE_ID, BULK_PROCESS_ID, PARENT_PRODUCT,
            BulkProcessData.STATUS_FAIL)).thenReturn(new ArrayList<>());
    }

    @AfterEach
    public void teardown() {
        Mockito.verifyNoMoreInteractions(bulkProcessDataService);
        Mockito.verifyNoMoreInteractions(bulkProcessImageQCService);
        Mockito.verifyNoMoreInteractions(productCategoryPredictionOutboundService);
        Mockito.verifyNoMoreInteractions(brandPredictionOutboundService);
        Mockito.verifyNoMoreInteractions(pcbOutboundService);
        Mockito.verifyNoMoreInteractions(objectMapper);
    }

    @Test
    public void process_HappyTest() throws Exception {
        Mockito.when(bulkProcessDataService.findByBulkProcessIdAndParentProductAndStatus(STORE_ID,
                BULK_PROCESS_ID, PARENT_PRODUCT, BulkProcessData.STATUS_PENDING))
            .thenReturn(bulkProcessDataList);
        Mockito.when(
            bulkProcessImageQCService.fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(
                STORE_ID, BULK_PROCESS_CODE, PARENT_PRODUCT)).thenReturn(bulkProcessImageQC);
        Mockito.when(productCategoryPredictionOutboundService.predictProductCategoriesByProductName(
            PRODUCT_NAME, EXTERNAL_CATEGORY, DS_RESPONSE)).thenReturn(CATEGORY_CODE);
        Mockito.when(pcbOutboundService.filterCategoryHierarchyByCategoryCodes(
            Collections.singletonList(CATEGORY_CODE))).thenReturn(categoryHierarchyResponses);
        Mockito.when(
                brandPredictionOutboundService.predictProductBrand(brandPredictionRequest,
                    DS_RESPONSE))
            .thenReturn(BRAND_NAME);
        Mockito.when(bulkProcessDataService.saveAndReturnBulkProcessData(Mockito.anyList()))
            .thenReturn(bulkProcessDataList);
        Mockito.doNothing().when(bulkProcessImageQCService).saveBulkProcessImageQc(Mockito.anyList());
        Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
            .thenReturn(rowDataJson);

        Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(TEST_DATA);
        categoryAndBrandPredictionServiceBean.process(brandAndCategoryPredictionRequest);

        Mockito.verify(bulkProcessDataService)
            .findByBulkProcessIdAndParentProductAndStatus(STORE_ID, BULK_PROCESS_ID, PARENT_PRODUCT,
                BulkProcessData.STATUS_PENDING);
        Mockito.verify(bulkProcessImageQCService)
            .fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(STORE_ID, BULK_PROCESS_CODE,
                PARENT_PRODUCT);
        Mockito.verify(productCategoryPredictionOutboundService)
            .predictProductCategoriesByProductName(PRODUCT_NAME, EXTERNAL_CATEGORY, DS_RESPONSE);
        Mockito.verify(pcbOutboundService)
            .filterCategoryHierarchyByCategoryCodes(Collections.singletonList(CATEGORY_CODE));
        Mockito.verify(brandPredictionOutboundService)
            .predictProductBrand(brandPredictionRequest, DS_RESPONSE);
        Mockito.verify(bulkProcessDataService).saveAndReturnBulkProcessData(Mockito.anyList());
        Mockito.verify(bulkProcessImageQCService).saveBulkProcessImageQc(Mockito.anyList());
        Mockito.verify(objectMapper)
            .readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
        Mockito.verify(objectMapper, Mockito.times(2)).writeValueAsString(Mockito.any());
        Mockito.verify(bulkProcessService)
          .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
        Mockito.verify(bulkProcessDataService)
          .findByBulkProcessIdAndParentProductAndStatus(STORE_ID, BULK_PROCESS_ID, PARENT_PRODUCT,
            BulkProcessData.STATUS_FAIL);
    }

    @Test
    public void process_categoryAndBrandPredictionThrowExceptionTest() throws Exception {
        bulkProcessDataList.getFirst().setErrorMessage("NOT EMPTY");
        Mockito.when(bulkProcessDataService.findByBulkProcessIdAndParentProductAndStatus(STORE_ID,
                BULK_PROCESS_ID, PARENT_PRODUCT, BulkProcessData.STATUS_PENDING))
            .thenReturn(bulkProcessDataList);
        Mockito.when(
            bulkProcessImageQCService.fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(
                STORE_ID, BULK_PROCESS_CODE, PARENT_PRODUCT)).thenReturn(bulkProcessImageQC);
        Mockito.when(productCategoryPredictionOutboundService.predictProductCategoriesByProductName(
            PRODUCT_NAME, EXTERNAL_CATEGORY, DS_RESPONSE)).thenThrow(new ApplicationRuntimeException(
            ErrorCategory.UNSPECIFIED, Constant.ERROR_IN_CATEGORY_PREDICTION));
        Mockito.when(
                brandPredictionOutboundService.predictProductBrand(brandPredictionRequest,
                    DS_RESPONSE))
            .thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, Constant.ERROR_IN_BRAND_PREDICTION));
        Mockito.when(bulkProcessDataService.saveAndReturnBulkProcessData(Mockito.anyList()))
            .thenReturn(bulkProcessDataList);
        Mockito.doNothing().when(bulkProcessImageQCService).saveBulkProcessImageQc(Mockito.anyList());
        Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class)))
            .thenReturn(rowDataJson);

        Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn(TEST_DATA);
        categoryAndBrandPredictionServiceBean.process(brandAndCategoryPredictionRequest);

        Mockito.verify(bulkProcessDataService)
            .findByBulkProcessIdAndParentProductAndStatus(STORE_ID, BULK_PROCESS_ID, PARENT_PRODUCT,
                BulkProcessData.STATUS_PENDING);
        Mockito.verify(bulkProcessImageQCService)
            .fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(STORE_ID, BULK_PROCESS_CODE,
                PARENT_PRODUCT);
        Mockito.verify(productCategoryPredictionOutboundService)
            .predictProductCategoriesByProductName(PRODUCT_NAME, EXTERNAL_CATEGORY, DS_RESPONSE);
        Mockito.verify(brandPredictionOutboundService)
            .predictProductBrand(brandPredictionRequest, DS_RESPONSE);
        Mockito.verify(bulkProcessDataService).saveAndReturnBulkProcessData(Mockito.anyList());
        Mockito.verify(bulkProcessImageQCService).saveBulkProcessImageQc(Mockito.anyList());
        Mockito.verify(objectMapper)
            .readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
        Mockito.verify(objectMapper, Mockito.times(2)).writeValueAsString(Mockito.any());
        Mockito.verify(bulkProcessService)
          .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
        Mockito.verify(bulkProcessDataService)
          .findByBulkProcessIdAndParentProductAndStatus(STORE_ID, BULK_PROCESS_ID, PARENT_PRODUCT,
            BulkProcessData.STATUS_FAIL);
    }

    @Test
    public void process_EmptyBulkProcessThrowExceptionTest() throws Exception {
        Mockito.when(bulkProcessDataService.findByBulkProcessIdAndParentProductAndStatus(STORE_ID,
                BULK_PROCESS_ID, PARENT_PRODUCT, BulkProcessData.STATUS_PENDING))
            .thenReturn(bulkProcessDataList);
        Mockito.when(
            bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(null);
        Mockito.when(
            bulkProcessImageQCService.fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(
                STORE_ID, BULK_PROCESS_CODE, PARENT_PRODUCT)).thenReturn(bulkProcessImageQC);
        Mockito.doNothing().when(bulkProcessImageQCService).saveBulkProcessImageQc(Mockito.anyList());

        Assertions.assertThrows(ApplicationRuntimeException.class,()->categoryAndBrandPredictionServiceBean.process(brandAndCategoryPredictionRequest));

        Mockito.verify(bulkProcessDataService)
            .findByBulkProcessIdAndParentProductAndStatus(STORE_ID, BULK_PROCESS_ID, PARENT_PRODUCT,
                BulkProcessData.STATUS_PENDING);
        Mockito.verify(bulkProcessImageQCService)
            .fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(STORE_ID, BULK_PROCESS_CODE,
                PARENT_PRODUCT);
        Mockito.verify(bulkProcessImageQCService).saveBulkProcessImageQc(Mockito.anyList());
        Mockito.verify(bulkProcessService)
            .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString());
        Mockito.verify(bulkProcessDataService)
            .findByBulkProcessIdAndParentProductAndStatus(STORE_ID, BULK_PROCESS_ID, PARENT_PRODUCT,
                BulkProcessData.STATUS_FAIL);
    }
}
