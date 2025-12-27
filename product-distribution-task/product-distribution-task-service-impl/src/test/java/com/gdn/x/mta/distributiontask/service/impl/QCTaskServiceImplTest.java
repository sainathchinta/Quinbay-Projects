package com.gdn.x.mta.distributiontask.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.gdn.x.mta.distributiontask.dao.api.VendorQuotaCounterRepository;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.ProductDTO;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;

import static org.mockito.ArgumentMatchers.eq;

/**
 * Created by Poornima on 9/26/16.
 */

@ExtendWith(MockitoExtension.class)
public class QCTaskServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String TASK_CODE = "TASK_CODE";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String CATEGORY_CODE = "CATEGORY_CODES";
  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String START_DATE = "2020-06-29 00:00";
  private static final String END_DATE = "2020-06-30 23:59";
  private static final String STATUS_ALL = "ALL";
  private static final String STATUS_PASSED = "PASSED";
  private static final String VENDOR = "Vendor1";
  private static final String ID = "31hj134-jbeeqw8-jkdsa2-ksdq234";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private ProductDTO productDTO;
  private ProductListRequest productListRequest;

  @InjectMocks
  private QCTaskServiceimpl qcTaskServiceimpl;

  @Mock
  private ProductService productService;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private TaskHistoryService taskHistoryService;

  @Mock
  private ProductDistributionTaskService productDistributionTaskService;

  @Mock
  private ApprovedProductPublisherService approvedProductPublisherService;

  @Mock
  private VendorQuotaCounterRepository vendorQuotaCounterRepository;

  @Mock
  private ProductReviewerService productReviewerService;

  @Mock
  private MandatoryRequestParam requestParam;

  @Mock
  private SolrVendorCollectionService solrVendorCollectionService;

  private ProductDistributionTask createPDT() {
    ProductDistributionTask pdt = new ProductDistributionTask();
    pdt.setTaskCode(TASK_CODE);
    pdt.setRejectedCount(2);
    pdt.setMarkForDelete(false);
    pdt.setProduct(buildProduct());
    return pdt;
  }

  private Product buildProduct() {
    return new Product.Builder().productName(PRODUCT_NAME).productCode(PRODUCT_CODE)
        .rejectedCount(2).productAttributes(new ArrayList<ProductAttribute>())
        .productImages(new ArrayList<ProductImage>()).productItems(new ArrayList<ProductItem>())
        .build();
  }

  @BeforeEach
  public void setUp() {
    ReflectionTestUtils.setField(qcTaskServiceimpl, "qcProductsRetryCount", 5);
    ReflectionTestUtils.setField(qcTaskServiceimpl, "qcProductsDeltaHours", 2);
    ReflectionTestUtils.setField(qcTaskServiceimpl, "qcProductsBatchSize", 100);
    ReflectionTestUtils.setField(qcTaskServiceimpl, "qcProductSkipStates", "REJECTED");

    productDTO = new ProductDTO();
    productDTO.setCategoryCode(CATEGORY_CODE);
    productDTO.setStoreId(STORE_ID);
    productDTO.setProductCode(PRODUCT_CODE);
    productDTO.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productDTO.setStartDate(new Date());
    productDTO.setEndDate(new Date());

    productListRequest = new ProductListRequest();
    productListRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productListRequest.setCategoryCode(CATEGORY_CODE);
    productListRequest.setProductCode(PRODUCT_CODE);
    productListRequest.setStartDate(START_DATE);
    productListRequest.setEndDate(END_DATE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.productDistributionTaskService);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.productServiceRepository);
    Mockito.verifyNoMoreInteractions(this.taskHistoryService);
    Mockito.verifyNoMoreInteractions(this.productReviewerService);
    Mockito.verifyNoMoreInteractions(this.solrVendorCollectionService);
  }

  @Test
   void testQcRejectProductTestByQCReturnToVendor() throws Exception {
    Product product = buildProduct();
    ProductDistributionTask productDistributionTask = createPDT();
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.any()))
        .thenReturn(createPDT());
    qcTaskServiceimpl.qcRejectProduct(buildProduct(), true, "", WorkflowState.REJECTED);
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .findByProductId(Mockito.any());
    Mockito.verify(this.productService, Mockito.times(1))
        .updateStateAndRemoveAssigneeDetails(Mockito.any(Product.class), eq(WorkflowState.REJECTED));
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1)).updateState(
        productDistributionTask, WorkflowState.REJECTED);
    Mockito.verify(this.productServiceRepository, Mockito.times(0))
        .republishToPDT(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).createTaskHistory(Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.anyString(), Mockito.any(), Mockito.anyString());
    Mockito.verify(this.productReviewerService).clearAllReviewerDetails(PRODUCT_CODE);
  }

  @Test
   void testQcRejectProductTestByQCReturnToDistributor() throws Exception {
    Product product = buildProduct();
    ProductDistributionTask productDistributionTask = createPDT();
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.any()))
        .thenReturn(createPDT());
    qcTaskServiceimpl.qcRejectProduct(buildProduct(), false, "", WorkflowState.QC_REJECTED);
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .findByProductId(Mockito.any());
    Mockito.verify(this.productService, Mockito.times(1))
        .updateState(Mockito.any(Product.class), eq(WorkflowState.UNASSIGNED));
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1)).updateState(
        Mockito.any(ProductDistributionTask.class), eq(WorkflowState.UNASSIGNED));
    Mockito.verify(this.productServiceRepository, Mockito.times(1))
        .republishToPDT(Mockito.any(), Mockito.any(), Mockito.anyString());
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).createTaskHistory(Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.anyString(), Mockito.any(), Mockito.anyString());
  }

  @Test
   void testApproveProductByQC() throws Exception {
    Mockito.when(this.productService.findByProductId(Mockito.any())).thenReturn(buildProduct());
    qcTaskServiceimpl.approveProductByQC(createPDT());
    Mockito.verify(this.productService, Mockito.times(1)).updateState(Mockito.any(),
        Mockito.any());
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1)).updateState(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.approvedProductPublisherService, Mockito.times(1))
        .publishVendorApprovedEvent(Mockito.any(), eq(false));
    Mockito.verify(this.productService, Mockito.times(1)).findByProductId(Mockito.any());
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).createTaskHistory(Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.anyString(), Mockito.any(), Mockito.anyString());
  }

  @Test
   void testmoveFailedProductToQC() throws Exception {
    ProductDistributionTask productDistributionTask = createPDT();
    productDistributionTask.setMarkForDelete(true);
    productDistributionTask.setState(WorkflowState.IN_REVIEW);
    qcTaskServiceimpl.moveFailedProductToQC(productDistributionTask);
    Mockito.verify(this.productService, Mockito.times(1)).updateState((Product) Mockito.any(),
        (WorkflowState) Mockito.any());
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1)).updateState(
        (ProductDistributionTask) Mockito.any(), (WorkflowState) Mockito.any());
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).createTaskHistory(Mockito.any(),
      Mockito.any(), Mockito.any(), Mockito.any(),
      Mockito.anyString(), Mockito.any(), Mockito.anyString());
  }

  @Test
   void testmoveFailedProductToQC_markfordelete_false() throws Exception {
    ProductDistributionTask productDistributionTask = createPDT();
    productDistributionTask.setState(WorkflowState.IN_REVIEW);
    qcTaskServiceimpl.moveFailedProductToQC(productDistributionTask);
    Mockito.verify(this.productService, Mockito.times(0)).updateState((Product) Mockito.any(),
        (WorkflowState) Mockito.any());
    Mockito.verify(this.productDistributionTaskService, Mockito.times(0)).updateState(
        (ProductDistributionTask) Mockito.any(), (WorkflowState) Mockito.any());
  }

  @Test
   void moveFailedProductToQCRejectedProductTest() throws Exception {
    ProductDistributionTask productDistributionTask = createPDT();
    productDistributionTask.setState(WorkflowState.REJECTED);
    qcTaskServiceimpl.moveFailedProductToQC(productDistributionTask);
    Mockito.verify(this.productService, Mockito.times(0)).updateState((Product) Mockito.any(),
      (WorkflowState) Mockito.any());
    Mockito.verify(this.productDistributionTaskService, Mockito.times(0)).updateState(
      (ProductDistributionTask) Mockito.any(), (WorkflowState) Mockito.any());
  }

  @Test
   void retryQcProductsTestEmptyParameter() {
    qcTaskServiceimpl.retryQcProducts(0, 0, 0);
    Mockito.verify(productService).republishFinalQcProductsForApproval(5, 2, 100);
  }

  @Test
   void retryQcProductsTestNegativeParameter() {
    qcTaskServiceimpl.retryQcProducts(-1, -1, -1);
    Mockito.verify(productService).republishFinalQcProductsForApproval(5, 2, 100);
  }

  @Test
   void retryQcProductsNonZeroQCRetryCountTest() {
    qcTaskServiceimpl.retryQcProducts(10, 0, 0);
    Mockito.verify(productService).republishFinalQcProductsForApproval(10, 2, 100);
  }

  @Test
   void retryQcProductsNonZeroDeltaHoursTest() {
    qcTaskServiceimpl.retryQcProducts(0, 50, 0);
    Mockito.verify(productService).republishFinalQcProductsForApproval(5, 50, 100);
  }

  @Test
   void retryQcProductsNonZeroBatchSizeTest() {
    qcTaskServiceimpl.retryQcProducts(0, 0, 200);
    Mockito.verify(productService).republishFinalQcProductsForApproval(5, 2, 200);
  }

  @Test
   void retryQcProductsParametersNonZeroTest() {
    qcTaskServiceimpl.retryQcProducts(50, 50, 50);
    Mockito.verify(productService).republishFinalQcProductsForApproval(50, 50, 50);
  }

  @Test
   void retryQcProductsParametersWhenUpdatedProductsNotEmptyTest() throws Exception {
    Product pdtProduct = new Product();
    pdtProduct.setProductCode(PRODUCT_CODE);
    pdtProduct.setMarkForDelete(Boolean.TRUE);
    Mockito.when(productService.republishFinalQcProductsForApproval(50, 50, 50))
        .thenReturn(Collections.singletonList(pdtProduct));
    qcTaskServiceimpl.retryQcProducts(50, 50, 50);
    Mockito.verify(productService).republishFinalQcProductsForApproval(50, 50, 50);
    Mockito.verify(this.solrVendorCollectionService).deleteProductFromSolr(Mockito.any());
  }

  @Test
   void retryQcProductsParametersWhenUpdatedProductsNotEmptyExceptionTest() throws Exception {
    Product pdtProduct = new Product();
    pdtProduct.setProductCode(PRODUCT_CODE);
    pdtProduct.setMarkForDelete(Boolean.TRUE);
    Mockito.when(productService.republishFinalQcProductsForApproval(50, 50, 50))
        .thenReturn(Collections.singletonList(pdtProduct));
    Mockito.doThrow(RuntimeException.class).when(solrVendorCollectionService)
        .deleteProductFromSolr(pdtProduct.getProductCode());
    qcTaskServiceimpl.retryQcProducts(50, 50, 50);
    Mockito.verify(productService).republishFinalQcProductsForApproval(50, 50, 50);
    Mockito.verify(this.solrVendorCollectionService).deleteProductFromSolr(Mockito.any());
  }

  @Test
   void getFilterProductSummaryFromSolrTest() throws Exception {
    List<Product> products = createProductList();
    Mockito.when(solrVendorCollectionService.getFilterProductSummary(eq(STORE_ID), eq(productListRequest),
        Mockito.any(), eq(PageRequest.of(PAGE, SIZE)))).thenReturn(new PageImpl<>(
        List.of(new ProductAndReviewerDetailsDTO(products.get(0), new ProductReviewer()))));
    qcTaskServiceimpl.getFilterProductSummaryFromSolr(STORE_ID, STATUS_ALL, productListRequest, PageRequest.of(PAGE, SIZE));
    Mockito.verify(solrVendorCollectionService).getFilterProductSummary(eq(STORE_ID),
        eq(productListRequest), Mockito.any(), Mockito.any());
    Mockito.verify(productService).setVendorForProductList(Mockito.any());
  }

  @Test
   void getFilterProductSummaryFromSolrStatusPassedTest() throws Exception {
    List<Product> products = createProductList();
    Mockito.when(
      solrVendorCollectionService.getFilterProductSummary(eq(STORE_ID), eq(productListRequest),
        Mockito.any(), eq(PageRequest.of(PAGE, SIZE)))).thenReturn(new PageImpl<>(
        List.of(new ProductAndReviewerDetailsDTO(products.get(0), new ProductReviewer()))));
    qcTaskServiceimpl.getFilterProductSummaryFromSolr(STORE_ID, STATUS_PASSED, productListRequest, PageRequest.of(PAGE, SIZE));
    Mockito.verify(solrVendorCollectionService).getFilterProductSummary(eq(STORE_ID), eq(productListRequest), Mockito.any(), Mockito.any());
    Mockito.verify(productService).setVendorForProductList(Mockito.any());
  }

  private List<Product> createProductList() {
    Product product =
        new Product.Builder().productName(PRODUCT_NAME).productCode(PRODUCT_CODE).productAttributes(new ArrayList<>())
            .productImages(new ArrayList<>()).productItems(new ArrayList<ProductItem>()).build();
    Vendor currentVendor = new Vendor();
    currentVendor.setName(VENDOR);
    currentVendor.setStoreId(STORE_ID);
    currentVendor.setUpdatedDate(new Date());
    currentVendor.setMarkForDelete(false);
    currentVendor.setAbleToReject(false);
    currentVendor.setQcRequired(false);
    currentVendor.setSlaInDays(10);
    currentVendor.setId(ID);
    product.setCurrentVendor(currentVendor);
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    return productList;
  }

}
