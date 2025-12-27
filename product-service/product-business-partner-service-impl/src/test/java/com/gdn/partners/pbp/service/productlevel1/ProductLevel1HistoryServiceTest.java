package com.gdn.partners.pbp.service.productlevel1;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.MDC;
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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.dto.workflow.product.ProductResubmitRequest;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class ProductLevel1HistoryServiceTest {

  private static final String PRODUCT_CODE = "MTA-0001";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String PRODUCT_UNASSIGNED_NOTES =
      "Product unassigned by : assignedBy";
  private static final String PRODUCT_BULK_ASSIGNED_NOTES =
      "Product bulk assigned to : assignedTo, and was assigned by : assignedBy";
  private static final String PRODUCT_ASSIGNED_NOTES =
      "Product assigned to : assignedTo, and was assigned by : assignedBy";
  private static final String EMPTY_NOTES =
      "Diubah : []";
  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_ID = "productId";
  private static final String OLD_PRODUCT_ID = "oldProductId";

  @Mock
  private ProductHistoryRepository productLevel1HistoryRepository;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @InjectMocks
  private ProductLevel1HistoryServiceBean productLevel1HistoryServiceBean;

  @Mock
  private ProductService productService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<ProductHistory> productHistoryArgumentCaptor;

  private ProductCollection generateProductCollection() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    return productCollection;
  }

  private ProductDetailResponse productDetailResponse;
  private ProductCreationRequest productCreationRequest;
  private ProductResubmitRequest productResubmitRequest;

  @Test
  public void getProductDiff() {
  }

  private ProductDetailResponse getProductOldProduct() {
    ProductDetailResponse oldProduct = new ProductDetailResponse();
    oldProduct.setName("testProduct1");
    oldProduct.setBrand("Samsung");
    oldProduct.setDescription("<p>some description</p>".getBytes());
    oldProduct.setHeight(1.2D);
    oldProduct.setLength(1.2D);
    oldProduct.setWeight(3D);
    oldProduct.setWidth(1.2D);
    oldProduct.setProductStory("<p>product story ....</p>");
    oldProduct.setUniqueSellingPoint("<p>usp</p>");
    oldProduct.setProductAttributeResponses(getOldProductAttributes());
    oldProduct.setProductItemResponses(getOldProductItems());
    return oldProduct;
  }

  private Set<ProductItemResponse> getOldProductItems() {
    ProductItemResponse item = new ProductItemResponse();
    item.setGeneratedItemName("Product Item1");
    item.setId("123");
    item.setDangerousGoodsLevel(2);
    item.setUpcCode("abc");
    Set<ProductItemResponse> itemList = new HashSet<>();
    itemList.add(item);
    return itemList;
  }

  private List<ProductAttributeResponse> getOldProductAttributes() {
    List<ProductAttributeResponse> productAttributeList = new ArrayList<>();
    ProductAttributeResponse attr1 = new ProductAttributeResponse();
    attr1.setProductAttributeName("product Attr1");
    attr1.setId("id1");
    ProductAttributeValueResponse val1 = new ProductAttributeValueResponse();
    val1.setId("123");
    val1.setDescriptiveAttributeValue("test");
    val1.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE);

    ProductAttributeValueResponse val2 = new ProductAttributeValueResponse();
    val2.setId("1234");
    val2.setDescriptiveAttributeValue("<p>test123</p>");
    PredefinedAllowedAttributeValueResponse predefinedVal = new PredefinedAllowedAttributeValueResponse();
    predefinedVal.setValue("<p>predefinedVal1</p>");
    val2.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.PREDEFINED);
    val2.setPredefinedAllowedAttributeValue(predefinedVal);
    List<ProductAttributeValueResponse> valueList = new ArrayList<>();
    valueList.add(val1);
    valueList.add(val2);
    attr1.setProductAttributeValues(valueList);
    productAttributeList.add(attr1);
    return productAttributeList;
  }

  private ProductCreationRequest getProduct() {
    ProductCreationRequest oldProduct = new ProductCreationRequest();
    oldProduct.setName("testProduct1");
    oldProduct.setBrand("Samsung");
    oldProduct.setDescription("<p>some description</p>".getBytes());
    oldProduct.setHeight(1.2D);
    oldProduct.setLength(1.2D);
    oldProduct.setWeight(3D);
    oldProduct.setWidth(1.2D);
    oldProduct.setProductStory("<p>product story ....</p>");
    oldProduct.setUniqueSellingPoint("<p>usp</p>");
    oldProduct.setProductAttributes(getProductAttributes());
    oldProduct.setProductItems(getProductItems());
    oldProduct.setProductCode(PRODUCT_CODE);
    oldProduct.setUpdatedBy(ASSIGNED_BY);
    return oldProduct;
  }

  private ProductResubmitRequest getProductResubmitRequest() {
    ProductResubmitRequest productResubmitRequest = new ProductResubmitRequest();
    ProductRequest productRequest = new ProductRequest();
    productRequest.setName("testProduct1");
    productRequest.setBrand("Samsung");
    productRequest.setDescription("<p>some description</p>".getBytes());
    productRequest.setHeight(1.2D);
    productRequest.setLength(1.2D);
    productRequest.setWeight(3D);
    productRequest.setWidth(1.2D);
    productRequest.setProductStory("<p>product story ....</p>");
    productRequest.setUniqueSellingPoint("<p>usp</p>");
    productRequest.setProductAttributes(getProductAttributes());
    productRequest.setProductItems(getProductItems());
    productRequest.setProductCode(PRODUCT_CODE);
    productRequest.setUpdatedBy(ASSIGNED_BY);
    productResubmitRequest.setProductRequest(productRequest);
    return productResubmitRequest;
  }

  private List<ProductItemRequest> getProductItems() {
    ProductItemRequest item = new ProductItemRequest();
    item.setGeneratedItemName("Product Item1");
    item.setId("123");
    item.setDangerousGoodsLevel(2);
    item.setUpcCode("abc");
    List<ProductItemRequest> itemList = new ArrayList<>();
    itemList.add(item);
    return itemList;
  }

  private List<ProductAttributeRequest> getProductAttributes() {
    List<ProductAttributeRequest> productAttributeList = new ArrayList<>();
    ProductAttributeRequest attr1 = new ProductAttributeRequest();
    attr1.setProductAttributeName("product Attr1");
    attr1.setId("id1");
    ProductAttributeValueRequest val1 = new ProductAttributeValueRequest();
    val1.setId("123");
    val1.setDescriptiveAttributeValue("test");
    val1.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE);

    ProductAttributeValueRequest val2 = new ProductAttributeValueRequest();
    val2.setId("1234");
    val2.setDescriptiveAttributeValue("<p>test123</p>");
    PredefinedAllowedAttributeValueRequest predefinedVal = new PredefinedAllowedAttributeValueRequest();
    predefinedVal.setValue("<p>predefinedVal1</p>");
    val2.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.PREDEFINED);
    val2.setPredefinedAllowedAttributeValue(predefinedVal);
    List<ProductAttributeValueRequest> valueList = new ArrayList<>();
    valueList.add(val1);
    valueList.add(val2);
    attr1.setProductAttributeValues(valueList);
    productAttributeList.add(attr1);
    return productAttributeList;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ProductCollection productCollection = this.generateProductCollection();
    productCollection.setProductId(PRODUCT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, "SYSTEM");
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.when(this.productLevel1HistoryRepository.save(Mockito.any(ProductHistory.class)))
        .thenReturn(null);
    productDetailResponse = getProductOldProduct();
    productCreationRequest = getProduct();
    productResubmitRequest = getProductResubmitRequest();
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel1HistoryRepository);
    Mockito.verifyNoMoreInteractions(this.productCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.productRepository, this.objectMapper);
    MDC.clear();
  }

  @Test
  public void createTest() throws Exception {
    this.productLevel1HistoryServiceBean.create("productCode", null, null);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productLevel1HistoryRepository).save(Mockito.any(ProductHistory.class));
  }

  @Test
  public void createTestSkipReview() throws Exception {
    ProductCollection productCollection = this.generateProductCollection();
    productCollection.setSkipReview(true);
    productCollection.setProductId(PRODUCT_ID);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    this.productLevel1HistoryServiceBean.create("productCode", null, null);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productLevel1HistoryRepository).save(Mockito.any(ProductHistory.class));
  }

  @Test
  public void createWithActivateStateTest() throws Exception {
    this.productLevel1HistoryServiceBean.create("productCode", "ACTIVATE", null);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productLevel1HistoryRepository).save(Mockito.any(ProductHistory.class));
  }

  @Test
  public void createHistoryTest() throws Exception {
    this.productLevel1HistoryServiceBean.create(new ProductCollection(), null, null);
    Mockito.verify(this.productLevel1HistoryRepository).save(Mockito.any(ProductHistory.class));
  }

  @Test
  public void findByStoreIdAndProductIdAndMarkForDeleteFalseTest() {
    productLevel1HistoryServiceBean.findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
    Mockito.verify(productLevel1HistoryRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void addProductHistoryProductUnassignmentTest() {
    productLevel1HistoryServiceBean
        .addProductHistoryForProductAssignment(Collections.singletonList(PRODUCT_CODE), Constants.DEFAULT_ASSIGNEE,
            ASSIGNED_BY);
    verify(productService).saveProductHistory(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, ASSIGNED_BY,
        SaveHistoryConstants.PRODUCT_UNASSIGNED, PRODUCT_UNASSIGNED_NOTES);
  }

  @Test
  public void addProductHistoryBulkAssignmentTest() {
    productLevel1HistoryServiceBean
        .addProductHistoryForProductAssignment(Arrays.asList(PRODUCT_CODE, Constants.PRODUCT_CODE), ASSIGNED_TO,
            ASSIGNED_BY);
    verify(productService).saveProductHistory(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, ASSIGNED_BY,
        SaveHistoryConstants.PRODUCT_ASSIGNED, PRODUCT_BULK_ASSIGNED_NOTES);
    verify(productService).saveProductHistory(Constants.DEFAULT_STORE_ID, Constants.PRODUCT_CODE, ASSIGNED_BY,
        SaveHistoryConstants.PRODUCT_ASSIGNED, PRODUCT_BULK_ASSIGNED_NOTES);
  }

  @Test
  public void addProductHistoryAssignmentTest() {
    productLevel1HistoryServiceBean
        .addProductHistoryForProductAssignment(Collections.singletonList(PRODUCT_CODE), ASSIGNED_TO,
            ASSIGNED_BY);
    verify(productService).saveProductHistory(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, ASSIGNED_BY,
        SaveHistoryConstants.PRODUCT_ASSIGNED, PRODUCT_ASSIGNED_NOTES);
  }

  @Test
  public void addHistoryForProductResubmittedTest() throws Exception {
    when(productRepository.findProductDetailByProductCode(PRODUCT_CODE)).thenReturn(productDetailResponse);
    productLevel1HistoryServiceBean.addHistoryForProductResubmitted(productCreationRequest);
    verify(productService).saveProductHistory(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, ASSIGNED_BY,
        SaveHistoryConstants.PRODUCT_RESUBMITTED, EMPTY_NOTES);
    verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
  }

  @Test
  public void addHistoryForProductResubmissionDueToContentOrImageRejectionTest() throws Exception {
    when(productRepository.findProductDetailByProductCode(PRODUCT_CODE)).thenReturn(productDetailResponse);
    productLevel1HistoryServiceBean
        .addHistoryForProductResubmissionDueToContentOrImageRejection(productResubmitRequest);
    verify(productService).saveProductHistory(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, ASSIGNED_BY,
        SaveHistoryConstants.PRODUCT_RESUBMITTED, EMPTY_NOTES);
    verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
  }

  @Test
  public void addHistoryForProductResubmittedExceptionTest() throws Exception {
    when(productRepository.findProductDetailByProductCode(PRODUCT_CODE)).thenReturn(null);
    try {
      productLevel1HistoryServiceBean.addHistoryForProductResubmitted(productCreationRequest);
    } catch (ApplicationException e) {
    } finally {
      verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    }
  }

  @Test
  public void addHistoryForProductResubmissionDueToContentOrImageRejectionExceptionTest() throws Exception {
    when(productRepository.findProductDetailByProductCode(PRODUCT_CODE)).thenReturn(null);
    try {
      productLevel1HistoryServiceBean
          .addHistoryForProductResubmissionDueToContentOrImageRejection(productResubmitRequest);
    } catch (ApplicationException e) {
    } finally {
      verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    }
  }

  @Test
  public void saveProductHistoryTest() {
    productLevel1HistoryServiceBean
        .saveProductHistory(PRODUCT_CODE, ASSIGNED_BY, SaveHistoryConstants.PRODUCT_ASSIGNED, EMPTY_NOTES);
    verify(productService).saveProductHistory(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, ASSIGNED_BY,
        SaveHistoryConstants.PRODUCT_ASSIGNED, EMPTY_NOTES);
  }

  @Test
  public void updateProductIdForRevisedProductsTest() {
    Mockito.doReturn(10).when(productLevel1HistoryRepository)
        .updateProductForResubmittedProducts(OLD_PRODUCT_ID, PRODUCT_ID,
            WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
    this.productLevel1HistoryServiceBean.updateProductIdForRevisedProducts(OLD_PRODUCT_ID, PRODUCT_ID);
    Mockito.verify(this.productLevel1HistoryRepository).updateProductForResubmittedProducts(OLD_PRODUCT_ID, PRODUCT_ID,
        WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
  }

  @Test
  public void updateProductIdForRevisedProductsRowsEffectedZeroTest() {
    Mockito.doReturn(0).when(productLevel1HistoryRepository)
        .updateProductForResubmittedProducts(OLD_PRODUCT_ID, PRODUCT_ID,
            WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
    this.productLevel1HistoryServiceBean.updateProductIdForRevisedProducts(OLD_PRODUCT_ID, PRODUCT_ID);
    Mockito.verify(this.productLevel1HistoryRepository).updateProductForResubmittedProducts(OLD_PRODUCT_ID, PRODUCT_ID,
        WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
  }

  @Test
  public void addProductHistoryBulkUnAssignmentTest() {
    Mockito.doNothing().when(productService).saveProductHistory(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, ASSIGNED_BY,
        SaveHistoryConstants.PRODUCT_UNASSIGNED, PRODUCT_UNASSIGNED_NOTES);
    productLevel1HistoryServiceBean
        .addProductHistoryForProductAssignment(Collections.singletonList(PRODUCT_CODE), StringUtils.EMPTY,
            ASSIGNED_BY);
    verify(productService).saveProductHistory(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, ASSIGNED_BY,
        SaveHistoryConstants.PRODUCT_UNASSIGNED, PRODUCT_UNASSIGNED_NOTES);
  }

  @Test
  public void createForNeedRevisionTest() throws Exception {
    NeedRevisionNotes notes =
        new NeedRevisionNotes(new ArrayList<>(), new ArrayList<>(), "notes", true, "notes", new ArrayList<>(), new ArrayList<>(),
            new ArrayList<>());
    this.productLevel1HistoryServiceBean.createForNeedRevision("productCode", null, null, notes,
        Constants.SYSTEM + Constants.HYPHEN + Constants.AUTO_NEED_REVISION);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productLevel1HistoryRepository).save(productHistoryArgumentCaptor.capture());
    verify(objectMapper).writeValueAsString(notes);
    Assertions.assertEquals(productHistoryArgumentCaptor.getValue().getCreatedBy(),
        Constants.SYSTEM + Constants.HYPHEN + Constants.AUTO_NEED_REVISION);
  }

  @Test
  public void createForNeedRevision_NullStoreIdTest() throws Exception {
    NeedRevisionNotes notes =
        new NeedRevisionNotes(new ArrayList<>(), new ArrayList<>(), "notes", true, "notes", new ArrayList<>(), new ArrayList<>(),
            new ArrayList<>());
    this.productLevel1HistoryServiceBean.createForNeedRevision("productCode", null, null, notes, StringUtils.EMPTY);
    verify(objectMapper).writeValueAsString(notes);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productLevel1HistoryRepository).save(Mockito.any(ProductHistory.class));
  }

  @Test
  public void createForNeedRevisionTest_Null() throws Exception {
    NeedRevisionNotes notes = null;
    this.productLevel1HistoryServiceBean.createForNeedRevision("productCode", null, null, notes, StringUtils.EMPTY);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.productLevel1HistoryRepository).save(Mockito.any(ProductHistory.class));
  }

  @Test
  public void createForNeedRevisionTest_NoId() throws Exception {
    ProductCollection productCollection = this.generateProductCollection();
    productCollection.setSkipReview(true);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    NeedRevisionNotes notes =
        new NeedRevisionNotes(new ArrayList<>(), new ArrayList<>(), "notes", true, "notes", new ArrayList<>(), new ArrayList<>(),
            new ArrayList<>());
    this.productLevel1HistoryServiceBean.createForNeedRevision("productCode", null, null, notes, StringUtils.EMPTY);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(objectMapper).writeValueAsString(notes);
  }

  @Test
  public void deleteProductHistoryByStoreIdAndProductIdTest() {
    productLevel1HistoryServiceBean.deleteProductHistoryByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(productLevel1HistoryRepository).deleteByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }
}
