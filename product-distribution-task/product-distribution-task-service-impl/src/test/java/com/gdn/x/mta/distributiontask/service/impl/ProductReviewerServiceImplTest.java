package com.gdn.x.mta.distributiontask.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.mta.distributiontask.dao.api.ProductReviewerRepository;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ProductReviewerServiceImplTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "storeId";
  private static final String ASSIGNEE = "assignee";
  private static final Date date = new Date();

  private ProductReviewer productReviewer;
  private final List<ProductReviewer> productReviewerList = new ArrayList<>();
  private List<String> productCodes;

  @Captor
  private ArgumentCaptor<ProductReviewer> productReviewerArgumentCaptor;

  @Mock
  ProductReviewerRepository productReviewerRepository;

  @InjectMocks
  ProductReviewerServiceImpl productReviewerService;

  @BeforeEach
  public void setUp() throws Exception {
    productReviewer = new ProductReviewer();
    productReviewer.setProductCode(PRODUCT_CODE);
    productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productReviewerRepository);
  }

  @Test
   void saveTest(){
    productReviewerService.save(productReviewer);
    Mockito.verify(productReviewerRepository).save(productReviewer);
  }

  @Test
   void findProductReviewerByProductCodesTest(){
    Mockito.when(productReviewerRepository.findByStoreIdAndProductCodeIn(STORE_ID,
        productCodes)).thenReturn(productReviewerList);
    productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, productCodes);
    Mockito.verify(productReviewerRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, productCodes);
  }

  @Test
   void findProductReviewerByStoreIdAndProductCodeMFDFalseTest(){
    Mockito.when(productReviewerRepository.findByStoreIdAndProductCode(STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    productReviewerService.findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerRepository)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
   void findProductReviewerByStoreIdAndProductCodeTest(){
    Mockito.when(productReviewerRepository.findByStoreIdAndProductCode(STORE_ID,
        PRODUCT_CODE)).thenReturn(productReviewer);
    productReviewerService.findProductReviewerByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productReviewerRepository)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
   void addNewProductTest(){
    Mockito.when(productReviewerRepository.save(Mockito.any(ProductReviewer.class)))
        .thenReturn(productReviewer);
    productReviewerService.addNewProduct(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productReviewerRepository).save(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE,
        productReviewerArgumentCaptor.getValue().getProductCode());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getAssignedDate());
  }

  @Test
   void resetAssignmentDataClearAssigneeTrueTest(){
    productReviewer.setMarkForDelete(true);
    productReviewer.setApproverAssignee(ASSIGNEE);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    Mockito.when(productReviewerRepository.save(Mockito.any(ProductReviewer.class)))
        .thenReturn(productReviewer);
    productReviewerService.resetAssignmentData(productReviewer, true);
    Mockito.verify(this.productReviewerRepository).save(productReviewerArgumentCaptor.capture());
    Assertions.assertFalse(productReviewerArgumentCaptor.getValue().isMarkForDelete());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getAssignedDate());
  }

  @Test
   void resetAssignmentDataClearAssigneeFalseTest(){
    productReviewer.setMarkForDelete(true);
    productReviewer.setApproverAssignee(ASSIGNEE);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    Mockito.when(productReviewerRepository.save(Mockito.any(ProductReviewer.class)))
        .thenReturn(productReviewer);
    productReviewerService.resetAssignmentData(productReviewer, false);
    Mockito.verify(this.productReviewerRepository).save(productReviewerArgumentCaptor.capture());
    Assertions.assertFalse(productReviewerArgumentCaptor.getValue().isMarkForDelete());
    Assertions.assertEquals(ASSIGNEE,
        productReviewerArgumentCaptor.getValue().getApproverAssignee());
    Assertions.assertNotNull(productReviewerArgumentCaptor.getValue().getAssignedDate());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void findProductReviewerByProductCodeTest() {
    Mockito.when(productReviewerRepository.findByProductCode(PRODUCT_CODE)).thenReturn(productReviewer);
    productReviewerService.findProductReviewerByProductCode(PRODUCT_CODE);
    Mockito.verify(productReviewerRepository).findByProductCode(PRODUCT_CODE);
  }

  @Test
   void clearExistingReviewDatesDetailsTest() {
    Mockito.when(productReviewerRepository.findByProductCode(PRODUCT_CODE)).thenReturn(productReviewer);
    productReviewerService.clearExistingReviewDatesDetails(PRODUCT_CODE);
    Mockito.verify(productReviewerRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productReviewerRepository).save(productReviewerArgumentCaptor.capture());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApprovedDate());
  }

  @Test
   void markForDeleteByProductCodeTest() {
    Mockito.when(productReviewerRepository.findByProductCode(PRODUCT_CODE)).thenReturn(productReviewer);
    productReviewerService.markForDeleteByProductCode(PRODUCT_CODE, "username");
    Mockito.verify(productReviewerRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productReviewerRepository).save(productReviewerArgumentCaptor.capture());
    Assertions.assertTrue(productReviewerArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
   void markForDeleteByProductCodeUsernameEmptyTest() {
    Mockito.when(productReviewerRepository.findByProductCode(PRODUCT_CODE)).thenReturn(productReviewer);
    productReviewerService.markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.verify(productReviewerRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productReviewerRepository).save(productReviewerArgumentCaptor.capture());
    Assertions.assertTrue(productReviewerArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
   void markForDeleteByProductCodeTestNullData() {
    Mockito.when(productReviewerRepository.findByProductCode(PRODUCT_CODE)).thenReturn(null);
    productReviewerService.markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.verify(productReviewerRepository).findByProductCode(PRODUCT_CODE);
  }

  @Test
   void clearAllReviewerDetailsTest() {
    Mockito.when(productReviewerRepository.findByProductCode(PRODUCT_CODE)).thenReturn(productReviewer);
    productReviewerService.clearAllReviewerDetails(PRODUCT_CODE);
    Mockito.verify(productReviewerRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productReviewerRepository).save(productReviewerArgumentCaptor.capture());
    Assertions.assertNull(productReviewerArgumentCaptor.getValue().getApproverAssignee());
  }

  @Test
   void clearAllReviewerDetailsNullData() {
    Mockito.when(productReviewerRepository.findByProductCode(PRODUCT_CODE)).thenReturn(null);
    productReviewerService.clearAllReviewerDetails(PRODUCT_CODE);
    Mockito.verify(productReviewerRepository).findByProductCode(PRODUCT_CODE);
  }

  @Test
   void updateProductImageAssignmentTest(){
    productReviewerService.updateProductAssignment(ASSIGNEE, date, productCodes);
    Mockito.verify(productReviewerRepository).updateProductAssignment(ASSIGNEE, date, productCodes);
  }

  @Test
   void deleteByProductCodesInTest(){
    productReviewerService.deleteByProductCodesIn(productCodes);
    Mockito.verify(productReviewerRepository).deleteByProductCodeIn(productCodes);
  }

  @Test
   void findProductReviewerByProductCodesInTest() {
    Mockito.when(this.productReviewerRepository.findByStoreIdAndProductCodeIn(STORE_ID,
      productCodes)).thenReturn(Collections.singletonList(productReviewer));
    List<ProductReviewer> productReviewers =
      productReviewerService.findProductReviewerByProductCodes(STORE_ID,
        Collections.singletonList(PRODUCT_CODE));
    Mockito.verify(this.productReviewerRepository).findByStoreIdAndProductCodeIn(STORE_ID,
      productCodes);
    Assertions.assertEquals(PRODUCT_CODE, productReviewers.get(0).getProductCode());
  }

  @Test
   void findProductReviewerByProductCodesIn_emptyProductCodesTest() {
    List<ProductReviewer> productReviewers =
      productReviewerService.findProductReviewerByProductCodes(STORE_ID, Collections.emptyList());
    Assertions.assertTrue(CollectionUtils.isEmpty(productReviewers));
  }

  @Test
   void findProductReviewerMapByProductCodesInTest() {
    Mockito.when(this.productReviewerRepository.findByStoreIdAndProductCodeIn(STORE_ID,
        productCodes)).thenReturn(Collections.singletonList(productReviewer));
    Map<String, ProductReviewer> productReviewers =
        productReviewerService.findProductReviewerMapByProductCodes(STORE_ID,
            Collections.singletonList(PRODUCT_CODE));
    Mockito.verify(this.productReviewerRepository).findByStoreIdAndProductCodeIn(STORE_ID,
        productCodes);
    Assertions.assertEquals(PRODUCT_CODE, productReviewers.get(PRODUCT_CODE).getProductCode());
  }

  @Test
   void findProductReviewerMapByProductCodesWithNullInTest() {
    Mockito.when(this.productReviewerRepository.findByStoreIdAndProductCodeIn(STORE_ID,
        productCodes)).thenReturn(null);
    Map<String, ProductReviewer> productReviewerMap =
        productReviewerService.findProductReviewerMapByProductCodes(STORE_ID,
            Collections.singletonList(PRODUCT_CODE));
    Mockito.verify(this.productReviewerRepository).findByStoreIdAndProductCodeIn(STORE_ID,
        productCodes);
    Assertions.assertTrue(CollectionUtils.isEmpty(productReviewerMap.keySet()));
  }

  @Test
   void findProductReviewerMapByProductCodesIn_emptyProductCodesTest() {
    Map<String, ProductReviewer> productReviewerMap =
        productReviewerService.findProductReviewerMapByProductCodes(STORE_ID, Collections.emptyList());
    Assertions.assertTrue(CollectionUtils.isEmpty(productReviewerMap.keySet()));
  }

}
