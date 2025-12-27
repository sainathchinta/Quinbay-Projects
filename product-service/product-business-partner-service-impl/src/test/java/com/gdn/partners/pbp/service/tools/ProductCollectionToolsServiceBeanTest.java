package com.gdn.partners.pbp.service.tools;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.repository.productlevel3.ProductLevel3WipRepository;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1SolrService;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;

public class ProductCollectionToolsServiceBeanTest {
  
  @InjectMocks
  ProductCollectionToolsServiceBean productCollectionToolsService;
  
  @Mock
  private ProductCollectionRepository productCollectionRepository;
  @Mock
  private ProductLevel3WipRepository productLevel3WipRepository;
  @Mock
  private ProductRepository productRepository;
  @Mock
  private ProductLevel1SolrService prdLv1SolrService;
  @Mock
  private ProductOutbound xProductOutbond;
  
  private static String REQUEST_ID = "as35f4asdf-asdf5as4df-as635fd4as6d5";
  private static String CATEGORY_CODE = "CAT-1234";
  private static String CATEGORY_NAME = "Cat Name";
  private static String PRODUCT_ID = "asdf21asd32f13a2sd1f23asd12sad1f";
  private static String PRODUCT_CODE = "MTA-123456";
  private static String PRODUCT_SKU = "SKU-103-12345-00001";
  private static String SPECIFIC_STATE = "DRAFT";
  private static String STATE_NEED_CORRECTION = "NEED_CORRECTION";
  private static String STATE_DRAFT = "DRAFT";
  private static String STORE_ID = "10001";
  private static String USERNAME = "agie@mail.com";
  
  private List<String> productCodes;
  private List<ProductLevel3Wip> prdLv3WipList;
  private ProductCollection productCollection;
  
  @BeforeEach
  public void setup() throws Exception {
    MockitoAnnotations.initMocks(this);
    
    this.productCodes = Arrays.asList(PRODUCT_CODE);
    productCollection = new ProductCollection();
    productCollection.setState(STATE_DRAFT);
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setProductId(PRODUCT_ID);
    productCollection.setUpdatedDate(new Date());
    
    ProductLevel3Wip prdLv3Wip = new ProductLevel3Wip();
    prdLv3Wip.setState(STATE_DRAFT);
    prdLv3Wip.setProductSku(PRODUCT_SKU);
    
    this.prdLv3WipList = Arrays.asList(prdLv3Wip);
    
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(productLevel3WipRepository.findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID))
      .thenReturn(prdLv3WipList);
  }
  
  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(productCollectionRepository);
    Mockito.verifyNoMoreInteractions(productLevel3WipRepository);
  }
  
  @Test
  public void syncState_StateIsSync_Success() throws Exception {
    productCollectionToolsService.syncState(productCodes, SPECIFIC_STATE, STORE_ID, USERNAME);
    
    Mockito.verify(productCollectionRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3WipRepository)
        .findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
  }
  
  @Test
  public void syncState_ProductCollectionNotFound_ThrowException() throws Exception {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(null);
    try{
      Assertions.assertThrows(Exception.class, () -> {
        productCollectionToolsService.syncState(productCodes, SPECIFIC_STATE, STORE_ID, USERNAME);
      });
    } catch(Exception e){
      throw e;
    }
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }
  
  @Test
  public void syncState_InvalidState_Success() throws Exception {
    productCollection.setState(STATE_NEED_CORRECTION);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    productCollectionToolsService.syncState(productCodes, SPECIFIC_STATE, STORE_ID, USERNAME);
    
    Mockito.verify(productCollectionRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
  }
  
  @Test
  public void syncState_StateNotSync_Success() throws Exception {
    productCollection.setState(STATE_NEED_CORRECTION);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    productCollectionToolsService.syncState(productCodes, STATE_NEED_CORRECTION, STORE_ID, USERNAME);
    
    Mockito.verify(productCollectionRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3WipRepository)
      .findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
    Mockito.verify(this.productLevel3WipRepository).save(Mockito.any(ProductLevel3Wip.class));
  }
  
  @Test
  public void syncState_ProductLv3WipNotFound_Success() throws Exception {
    Mockito.when(productLevel3WipRepository.findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID))
      .thenReturn(null);
    productCollectionToolsService.syncState(productCodes, SPECIFIC_STATE, STORE_ID, USERNAME);
    
    Mockito.verify(productCollectionRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3WipRepository)
      .findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
  }
  
  @Test
  public void moveProductCollectionCategory_Valid_Success() throws Exception {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    
    CategorySummaryResponse catResp = new CategorySummaryResponse();
    catResp.setCategoryCode(CATEGORY_CODE);
    catResp.setCategoryName(CATEGORY_NAME);
    Mockito.when(productRepository.movePrdCategoryByPrdCode(REQUEST_ID, USERNAME, PRODUCT_CODE, CATEGORY_CODE))
      .thenReturn(new GdnRestSingleResponse<CategorySummaryResponse>(catResp, REQUEST_ID));
    
    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(null);
    Mockito.doNothing().when(prdLv1SolrService).update(PRODUCT_CODE);
    
    productCollectionToolsService.moveProductCollectionCategory(REQUEST_ID, USERNAME, 
        STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productRepository).movePrdCategoryByPrdCode(REQUEST_ID, USERNAME, PRODUCT_CODE, CATEGORY_CODE);
    Mockito.verify(productCollectionRepository).save(Mockito.any(ProductCollection.class));
    Mockito.verify(prdLv1SolrService).update(PRODUCT_CODE);
  }
  
  @Test
  public void moveProductCollectionCategory_ProductNotFound_Success() throws Exception {
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(null);
    try{
      Assertions.assertThrows(Exception.class, () -> {
        productCollectionToolsService.moveProductCollectionCategory(REQUEST_ID, USERNAME,
            STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
      });
    } catch(Exception e){
      throw e;
    }
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }
  
  @Test
  public void addProductAttributesByProductCode_Valid_Success() throws Exception {
    ProductAttributeResponse prdAttrResp = new ProductAttributeResponse();
    prdAttrResp.setAttribute(new AttributeResponse());
    prdAttrResp.setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    
    ProductAttributeValueResponse prdAttrValueResp = new ProductAttributeValueResponse();
    prdAttrResp.getProductAttributeValues().add(prdAttrValueResp);
    Mockito.when(productRepository.addProductAttributesByProductCode(Mockito.anyString(), Mockito.anyString(), 
        Mockito.any())).thenReturn(new GdnRestListResponse<>(Arrays.asList(prdAttrResp), null, REQUEST_ID));
    
    Mockito.when(xProductOutbond.addProductAttribute(Mockito.anyString(), Mockito.anyString(), 
        Mockito.any(), Mockito.anyString())).thenReturn(new GdnBaseRestResponse());
    
    AddProductAttributesRequest request = new AddProductAttributesRequest();
    productCollectionToolsService.addProductAttributesByProductCode(REQUEST_ID, USERNAME, request);
    
    Mockito.verify(productRepository).addProductAttributesByProductCode(Mockito.anyString(), Mockito.anyString(), 
        Mockito.any());
    Mockito.verify(xProductOutbond, Mockito.atLeastOnce()).addProductAttribute(Mockito.anyString(), Mockito.anyString(), 
        Mockito.any(), Mockito.any());
  }
  
  @Test
  public void addProductAttributesByProductCode_NullAttr_Success() throws Exception {
    ProductAttributeResponse prdAttrResp = new ProductAttributeResponse();
    prdAttrResp.setAttribute(new AttributeResponse());
    prdAttrResp.setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    
    ProductAttributeValueResponse prdAttrValueResp = new ProductAttributeValueResponse();
    prdAttrResp.getProductAttributeValues().add(prdAttrValueResp);
    Mockito.when(productRepository.addProductAttributesByProductCode(Mockito.anyString(), Mockito.anyString(), 
        Mockito.any())).thenReturn(new GdnRestListResponse<>(null, null, REQUEST_ID));
    
    AddProductAttributesRequest request = new AddProductAttributesRequest();
    productCollectionToolsService.addProductAttributesByProductCode(REQUEST_ID, USERNAME, request);
    
    Mockito.verify(productRepository).addProductAttributesByProductCode(Mockito.anyString(), Mockito.anyString(), 
        Mockito.any());
  }

  @Test
  public void productCollectionUpdatedDateTest() throws Exception {
    Date date = new Date();
    productCollection.setUpdatedDate(date);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    CategorySummaryResponse catResp = new CategorySummaryResponse();
    catResp.setCategoryCode(CATEGORY_CODE);
    catResp.setCategoryName(CATEGORY_NAME);
    Mockito.when(productRepository.movePrdCategoryByPrdCode(REQUEST_ID, USERNAME, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(catResp, REQUEST_ID));

    Mockito.when(productCollectionRepository.save(Mockito.any(ProductCollection.class))).thenReturn(null);
    Mockito.doNothing().when(prdLv1SolrService).update(PRODUCT_CODE);

    productCollectionToolsService
        .moveProductCollectionCategory(REQUEST_ID, USERNAME, STORE_ID, PRODUCT_CODE, CATEGORY_CODE);

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productRepository).movePrdCategoryByPrdCode(REQUEST_ID, USERNAME, PRODUCT_CODE, CATEGORY_CODE);
    Mockito.verify(productCollectionRepository).save(Mockito.any(ProductCollection.class));
    Mockito.verify(prdLv1SolrService).update(PRODUCT_CODE);
    Assertions.assertEquals(date.toString(), productCollection.getUpdatedDate().toString());
  }
}
