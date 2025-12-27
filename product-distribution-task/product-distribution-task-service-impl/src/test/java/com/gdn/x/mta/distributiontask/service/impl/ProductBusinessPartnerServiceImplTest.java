package com.gdn.x.mta.distributiontask.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductWorkflowRepository;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

/**
 * Created by Alok on 10/13/16.
 */

@ExtendWith(MockitoExtension.class)
public class ProductBusinessPartnerServiceImplTest {

  private static final String REQUEST_ID = "request-id";
  private static final String USER_NAME = "user-name";
  private static final String PRODUCT_CODE = "PDT-1234";

  @InjectMocks 
  private ProductBusinessPartnerServiceImpl productBusinessPartnerService;

  @Mock private ProductServiceRepository productServiceRepository;
  
  @Mock
  private ProductWorkflowRepository productWorkflowRepository;

  @Test public void deleteProductCollectionTestOk() throws Exception {
    RejectProductDTO rejectProductDTO = new RejectProductDTO();
    productBusinessPartnerService.deleteProductCollection(REQUEST_ID, USER_NAME, rejectProductDTO);
    Mockito.verify(productServiceRepository)
        .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(true),
            Mockito.any(RejectProductDTO.class));
  }

  @Test
   void deleteProductCollection_success() throws Exception {
    RejectProductDTO rejectProductDTO = new RejectProductDTO();
    productBusinessPartnerService
        .deleteProductCollection(REQUEST_ID, USER_NAME, false, rejectProductDTO);
    Mockito.verify(productServiceRepository)
        .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(false),
            Mockito.any(RejectProductDTO.class));
  }


  @Test public void republishToPDTTestOk() throws Exception {
    productBusinessPartnerService.republishToPDT(REQUEST_ID, USER_NAME, "product-code");
    Mockito.verify(productServiceRepository)
        .republishToPDT(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }


  @Test public void deleteProductCollection() throws Exception {
    Mockito.doNothing().when(productServiceRepository)
        .deleteProductCollection(Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
            (RejectProductDTO) Mockito.any());
    productBusinessPartnerService
        .deleteProductCollection(REQUEST_ID, USER_NAME, new RejectProductDTO());
    Mockito.verify(productServiceRepository)
        .deleteProductCollection(Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
            (RejectProductDTO) Mockito.any());
  }

  @Test 
  public void getWorkflowStatus() throws Exception {
    Mockito.when(productWorkflowRepository
        .getWorkflowStatus(PRODUCT_CODE)).thenReturn(new ProductWorkflowStatusResponse());
    productBusinessPartnerService.getWorkflowStatus(PRODUCT_CODE);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE);
  }
  
  @Test public void republishToPDT() throws Exception {
    Mockito.doNothing().when(productServiceRepository)
        .republishToPDT(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    productBusinessPartnerService.republishToPDT(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    Mockito.verify(productServiceRepository).republishToPDT(REQUEST_ID, USER_NAME, PRODUCT_CODE);
  }

  @Test public void getProductDetailsByProductCodes() throws Exception {
    Mockito.when(productServiceRepository
        .findProductDetailsByProductCodes(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList())).thenReturn(new ArrayList<ProductDetailResponse>());
    productBusinessPartnerService
        .getProductDetailsByProductCodes(REQUEST_ID, USER_NAME, List.of(PRODUCT_CODE));
    Mockito.verify(productServiceRepository)
        .findProductDetailsByProductCodes(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList());
  }

}
