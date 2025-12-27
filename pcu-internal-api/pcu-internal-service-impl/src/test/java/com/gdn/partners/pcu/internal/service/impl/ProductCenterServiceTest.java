package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
//import org.powermock.api.mockito.PowerMockito;
//import org.powermock.core.classloader.annotations.PrepareForTest;
import org.springframework.data.domain.Page;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.product.DownloadUnmappedSkuDomainEventModel;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.feign.XProductFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.web.model.request.CurrentCategoryCatalogWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterListingActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.request.SalesCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterSummaryWebResponse;
import com.gdn.x.product.model.vo.MasterCategoryResponse;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCategoryDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ProductCenterServiceTest {

  @Mock
  private XProductFeign xProductFeign;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private ProductCenterServiceImpl productCenterService;

  private static final String CATALOG_CODE = "12501";
  private static final String CATEGORY_CODE = "CAT_CODE";
  private static final String MERCHANT_CODE = "MERCHANT_CODE";
  private static final String ACTIVE = "Active";
  private static final String SUSPENDED = "Suspended";
  private static final String CATEGORY_NAME = "CAT_NAME";
  private static final String KEYWORD = "KEYWORD";
  private static final String PRODUCT_SKU = "CAT_CODE";
  private static final String PRODUCT_NAME = "KEYWORD";
  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String ETD_NOTE = "ETD_NOTE";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 100;
  private static final String PRODUCT_CENTER_ACTIVITY = "activity";
  private static final String PRODUCT_CENTER_DESCRIPTION = "description";
  private static final String PRODUCT_CENTER_UPDATEDBY = "updatedBy";

  private ProductCenterSummaryWebRequest productCenterSummaryWebRequest;
  private ProductCenterSummaryResponse productCenterSummaryResponse;
  private ProductCenterListingActionWebRequest productCenterListingActionWebRequest;
  private ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
  private ProductResponse productResponse = new ProductResponse();
  private ProductCenterDetailResponse productCenterDetailResponse;
  private CategoryNamesResponse categoryNamesResponse;
  private SalesCategoryMappingUpdateRequest salesCategoryMappingUpdateRequest;
  private SalesCategoryMappingWebRequest salesCategoryMappingWebRequest;
  private ProductCenterHistoryResponse productCenterHistoryResponse;
  private ProductL3Response productL3Response;
  private MockedStatic<Credential> mockedStatic;

  @BeforeEach
  public void setUp() {
    productCenterSummaryWebRequest = new ProductCenterSummaryWebRequest(CATEGORY_CODE, KEYWORD);

    productCenterSummaryResponse = new ProductCenterSummaryResponse();
    productCenterSummaryResponse.setProductSku(PRODUCT_SKU);
    productCenterSummaryResponse.setProductName(PRODUCT_NAME);
    productCenterSummaryResponse.setMasterCategory(new MasterCategoryResponse(CATEGORY_CODE, CATEGORY_NAME));
    productCenterSummaryResponse.setSalesCategories(Arrays.asList(CATEGORY_CODE));

    productCenterListingActionWebRequest = new ProductCenterListingActionWebRequest();
    productCenterListingActionWebRequest.setNewCatalogCode(CATALOG_CODE);
    productCenterListingActionWebRequest.setNewCategoryCode(CATEGORY_CODE);
    productCenterListingActionWebRequest.setCurrentCategoryCodeSelected(Arrays
        .asList(new CurrentCategoryCatalogWebRequest(CATALOG_CODE, CATEGORY_CODE),
            new CurrentCategoryCatalogWebRequest(CATALOG_CODE, CATEGORY_CODE)));
    productCenterListingActionWebRequest.setProductSKus(Arrays.asList(PRODUCT_SKU));

    ItemCategoryDTO itemCategoryDTO = new ItemCategoryDTO();
    itemCategoryDTO.setCategory(CATEGORY_NAME);
    itemCategoryDTO.setLevel(1);
    itemCategoryDTO.setProductCategoryCode(CATEGORY_CODE);
    productResponse.setProductSku(PRODUCT_SKU);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    MasterDataProductImageDTO masterDataProductImageDTO = new MasterDataProductImageDTO();
    masterDataProductImageDTO.setMainImage(true);
    masterDataProductDTO.setMasterDataProductImages(Collections.singletonList(masterDataProductImageDTO));
    masterDataProductDTO.setProductName(PRODUCT_NAME);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    productResponse.setProductCode(PRODUCT_SKU);
    productResponse.setUpdatedDate(new Date());
    productResponse.setMasterCatalog(new MasterCatalogDTO(CATALOG_CODE, new CategoryDTO(CATEGORY_CODE, CATEGORY_CODE)));
    ItemCatalogDTO itemCatalogDTO = new ItemCatalogDTO(CATALOG_CODE, Collections.singletonList(itemCategoryDTO));
    ItemCatalogDTO itemCatalogDTO1 = new ItemCatalogDTO("10001", Collections.singletonList(itemCategoryDTO));
    productResponse.setItemCatalogs(Arrays.asList(itemCatalogDTO, itemCatalogDTO1));
    productResponse.setMerchantCode(MERCHANT_CODE);
    productAndItemsResponse.setProduct(productResponse);
    productAndItemsResponse.setItems(new ArrayList<>());

    productCenterDetailResponse =
        ProductCenterDetailResponse.builder().imagePath("abc.jpg").sellerName(MERCHANT_CODE).status(ACTIVE)
            .markForDelete(false).productCenterUpdatedDate(new Date()).productName(PRODUCT_NAME).build();

    categoryNamesResponse = new CategoryNamesResponse();
    Map<String, String> category = new HashMap<>();
    category.put(CATEGORY_CODE, CATEGORY_NAME);
    categoryNamesResponse.setCategoryMap(category);

    salesCategoryMappingUpdateRequest = SalesCategoryMappingUpdateRequest.builder().addedCategories(new ArrayList<>())
        .deletedCategories(new ArrayList<>()).build();

    salesCategoryMappingWebRequest = SalesCategoryMappingWebRequest.builder().addedCategories(new ArrayList<>())
        .deletedCategories(new ArrayList<>()).build();
    productCenterHistoryResponse =
        ProductCenterHistoryResponse.builder().activity(PRODUCT_CENTER_ACTIVITY).description(PRODUCT_CENTER_DESCRIPTION)
            .updatedBy(PRODUCT_CENTER_UPDATEDBY).productSku(PRODUCT_SKU).updatedDate(new Date()).build();

    productL3Response = new ProductL3Response();
    productL3Response.setMasterDataProduct(masterDataProductDTO);
    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode(CATEGORY_CODE);
    masterCatalogDTO.setCategory(categoryDTO);
    productL3Response.setMasterCatalog(masterCatalogDTO);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(xProductFeign, pcbFeign);
    if (mockedStatic != null) {
      mockedStatic.close();
    }
  }

  private void mockStaticMethods(String[] accessibility) {
    mockedStatic = Mockito.mockStatic(Credential.class);
    mockedStatic.when(Credential::getAccessibilities).thenReturn(accessibility);
  }

  @Test
  public void getProductCenterFilterSummaryTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    when(xProductFeign.getProductCenterSummaryFilter(STORE_ID, REQUEST_ID, PAGE, SIZE,
        RequestHelper.toProductCenterSummaryRequest(productCenterSummaryWebRequest))).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(productCenterSummaryResponse), new PageMetaData(PAGE, SIZE, 1),
            REQUEST_ID));
    Page<ProductCenterSummaryWebResponse> response = productCenterService
        .getProductCenterFilterSummary(STORE_ID, REQUEST_ID, productCenterSummaryWebRequest, PAGE, SIZE);
    Assertions.assertEquals(PRODUCT_SKU, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getMasterCategory().getCategoryCode());
    verify(xProductFeign).getProductCenterSummaryFilter(STORE_ID, REQUEST_ID, PAGE, SIZE,
        RequestHelper.toProductCenterSummaryRequest(productCenterSummaryWebRequest));
  }

  @Test
  public void getProductCenterFilterSummaryAccessibilityTest() throws Exception {
    when(xProductFeign.getProductCenterSummaryFilter(STORE_ID, REQUEST_ID, PAGE, SIZE,
        RequestHelper.toProductCenterSummaryRequest(productCenterSummaryWebRequest))).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(productCenterSummaryResponse), new PageMetaData(PAGE, SIZE, 1),
            REQUEST_ID));
    try {
      Page<ProductCenterSummaryWebResponse> response =
          productCenterService.getProductCenterFilterSummary(STORE_ID, REQUEST_ID,
              productCenterSummaryWebRequest, PAGE, SIZE);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void updateProductCenterListingAddTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    productCenterListingActionWebRequest.setAction("ADD");
    when(xProductFeign.addProductSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    productCenterService.updateProductCenterListing(STORE_ID, REQUEST_ID, productCenterListingActionWebRequest);
    verify(xProductFeign).addProductSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void updateProductCenterListingDeleteTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    productCenterListingActionWebRequest.setAction("DELETE");
    when(xProductFeign.deleteSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    productCenterService.updateProductCenterListing(STORE_ID, REQUEST_ID, productCenterListingActionWebRequest);
    verify(xProductFeign, times(2)).deleteSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void updateProductCenterListingMoveTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    productCenterListingActionWebRequest.setAction("MOVE");
    when(xProductFeign.moveProductSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE, CATEGORY_CODE,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    when(xProductFeign.deleteSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    productCenterService.updateProductCenterListing(STORE_ID, REQUEST_ID, productCenterListingActionWebRequest);
    verify(xProductFeign)
        .moveProductSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE, CATEGORY_CODE,
            new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
    verify(xProductFeign).deleteSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));

  }

  @Test
  public void updateProductCenterListingMoveOneCatgeoryTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    productCenterListingActionWebRequest.setAction("MOVE");
    productCenterListingActionWebRequest.setCurrentCategoryCodeSelected(
        (Arrays.asList(new CurrentCategoryCatalogWebRequest(CATALOG_CODE, CATEGORY_CODE))));
    when(xProductFeign.moveProductSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE, CATEGORY_CODE,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    productCenterService.updateProductCenterListing(STORE_ID, REQUEST_ID, productCenterListingActionWebRequest);
    verify(xProductFeign)
        .moveProductSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE, CATEGORY_CODE,
            new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
  }


  @Test
  public void updateProductCenterListingMoveAccessibilityTest() throws Exception {
    when(xProductFeign.moveProductSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE, CATEGORY_CODE,
        new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    try {
      productCenterService.updateProductCenterListing(STORE_ID, REQUEST_ID,
          productCenterListingActionWebRequest);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void getProductCenterHistoryByProductSkuTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    when(this.xProductFeign.getProductCenterHistory(PRODUCT_SKU, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(productCenterHistoryResponse), new PageMetaData(),
            REQUEST_ID));
    Page<ProductCenterHistoryWebResponse> response = productCenterService.getProductCenterHistoryByProductSku(PRODUCT_SKU, PAGE, SIZE);
    verify(this.xProductFeign).getProductCenterHistory(PRODUCT_SKU, PAGE, SIZE);
    Assertions.assertEquals(PRODUCT_CENTER_ACTIVITY, response.getContent().get(0).getActivity());
    Assertions.assertEquals(PRODUCT_SKU, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CENTER_DESCRIPTION, response.getContent().get(0).getDescription());
    Assertions.assertEquals(PRODUCT_CENTER_UPDATEDBY, response.getContent().get(0).getUser());
  }

  @Test
  public void getProductCenterHistoryByProductSkuAccessibilityFailTest() throws Exception {
    try{
    productCenterService.getProductCenterHistoryByProductSku(PRODUCT_SKU, PAGE, SIZE);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void getProductCenterDetailTest() {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    when(xProductFeign.getProductDetailsByProductSku(STORE_ID, REQUEST_ID, PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(pcbFeign.getCategoryNames(any(CategoryMultipleIdRequest.class), anyInt(), anyInt())).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, categoryNamesResponse, REQUEST_ID));
    ProductCenterDetailWebResponse response =
        this.productCenterService.getProductCenterDetail(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    verify(xProductFeign).getProductDetailsByProductSku(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    verify(pcbFeign).getCategoryNames(any(CategoryMultipleIdRequest.class), anyInt(), anyInt());
    Assertions.assertNotNull(response);
    Assertions.assertEquals(ACTIVE, response.getStatus());
    Assertions.assertEquals(CATEGORY_CODE, response.getMasterCategory().getCategoryCode());
  }

  @Test
  public void getProductCenterDetailEtdNotesTest() {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    productL3Response.setEtdNotes(ETD_NOTE);
    productL3Response.setSuspended(true);
    ItemCatalogDTO itemCatalogDTO = new ItemCatalogDTO();
    itemCatalogDTO.setCatalogId(CATALOG_CODE);
    ItemCategoryDTO itemCategoryDTO = new ItemCategoryDTO();
    itemCategoryDTO.setCategoryId(CATEGORY_CODE);
    itemCatalogDTO.setItemCategories(Collections.singletonList(itemCategoryDTO));
    productL3Response.setItemCatalogs(Collections.singletonList(itemCatalogDTO));
    productL3Response.setMasterCatalog(null);
    when(xProductFeign.getProductDetailsByProductSku(STORE_ID, REQUEST_ID, PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(pcbFeign.getCategoryNames(any(CategoryMultipleIdRequest.class), anyInt(), anyInt())).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, categoryNamesResponse, REQUEST_ID));
    ProductCenterDetailWebResponse response =
        this.productCenterService.getProductCenterDetail(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    verify(xProductFeign).getProductDetailsByProductSku(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    verify(pcbFeign).getCategoryNames(any(CategoryMultipleIdRequest.class), anyInt(), anyInt());
    Assertions.assertNotNull(response);
    Assertions.assertEquals(SUSPENDED, response.getStatus());
  }

  @Test
  public void getProductCenterDetailTest_2() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    productAndItemsResponse.getProduct().setMasterCatalog(null);
    when(xProductFeign.getProductDetailsByProductSku(STORE_ID, REQUEST_ID, PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(pcbFeign.getCategoryNames(any(CategoryMultipleIdRequest.class), anyInt(), anyInt())).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, categoryNamesResponse, REQUEST_ID));
    ProductCenterDetailWebResponse response =
        this.productCenterService.getProductCenterDetail(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    verify(xProductFeign).getProductDetailsByProductSku(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    verify(pcbFeign).getCategoryNames(any(CategoryMultipleIdRequest.class), anyInt(), anyInt());
    Assertions.assertNotNull(response);
    Assertions.assertEquals(ACTIVE, response.getStatus());
  }

  @Test
  public void getProductCenterDetailTest_3() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    productResponse.setMasterCatalog(new MasterCatalogDTO("10001", new CategoryDTO(CATEGORY_CODE, CATEGORY_CODE)));
    when(xProductFeign.getProductDetailsByProductSku(STORE_ID, REQUEST_ID, PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(pcbFeign.getCategoryNames(any(CategoryMultipleIdRequest.class), anyInt(), anyInt())).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, categoryNamesResponse, REQUEST_ID));
    ProductCenterDetailWebResponse response =
        this.productCenterService.getProductCenterDetail(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    verify(xProductFeign).getProductDetailsByProductSku(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    verify(pcbFeign).getCategoryNames(any(CategoryMultipleIdRequest.class), anyInt(), anyInt());
    Assertions.assertNotNull(response);
    Assertions.assertEquals(ACTIVE, response.getStatus());
  }

  @Test
  public void getProductCenterDetail_ApplicationRuntimeExceptionTest() throws Exception {
    try {
      productCenterService.getProductCenterDetail(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void updateProductCenterDetailTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    when(xProductFeign.updateSalesCategory(STORE_ID, REQUEST_ID, PRODUCT_SKU, salesCategoryMappingUpdateRequest))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    productCenterService.updateProductCenterDetail(STORE_ID, REQUEST_ID, PRODUCT_SKU, salesCategoryMappingWebRequest);
    verify(xProductFeign).updateSalesCategory(STORE_ID, REQUEST_ID, PRODUCT_SKU, salesCategoryMappingUpdateRequest);
  }

  @Test
  public void updateProductCenterDetail_ApplicationRuntimeExceptionTest() throws Exception {
    try {
      productCenterService.updateProductCenterDetail(STORE_ID, REQUEST_ID, PRODUCT_SKU,
          new SalesCategoryMappingWebRequest());
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void downloadUnmappedSkusTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    productCenterService.downloadUnmappedSkus(STORE_ID, REQUEST_ID, USERNAME, CATEGORY_CODE, "EN");
    verify(kafkaPublisher).send(DomainEventName.DOWNLOAD_UNMAPPED_SKUS, USERNAME,
        new DownloadUnmappedSkuDomainEventModel(STORE_ID, USERNAME, USERNAME, REQUEST_ID, CATEGORY_CODE, "EN"));
  }

  @Test
  public void downloadUnmappedSkusAcessibilityTest() throws Exception {
    String[] accessibility = {};
    mockStaticMethods(accessibility);
    try{
      productCenterService.downloadUnmappedSkus(STORE_ID, REQUEST_ID, USERNAME, CATEGORY_CODE, "EN");
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }
}
