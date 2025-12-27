package com.gdn.x.mta.distributiontask.service.impl;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductWorkflowRepository;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.service.api.ProductBusinessPartnerService;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

/**
 * Created by virajjasani on 25/09/16.
 */
@Service
public class ProductBusinessPartnerServiceImpl implements ProductBusinessPartnerService {

  @Autowired
  private ProductServiceRepository productServiceRepository;
  
  @Autowired
  private ProductWorkflowRepository productWorkflowRepository;

  @Override
  public void deleteProductCollection(String requestId, String userName,
      RejectProductDTO rejectProductDTO) throws Exception {
    this.deleteProductCollection(requestId, userName, true, rejectProductDTO);
  }

  @Override
  public void deleteProductCollection(String requestId, String userName,
      boolean needEmailNotification, RejectProductDTO rejectProductDTO) throws Exception {
    this.productServiceRepository
        .deleteProductCollection(requestId, userName, needEmailNotification, rejectProductDTO);
  }

  @Override
  public void republishToPDT(String requestId, String userName, String productCode)
      throws Exception {
    this.productServiceRepository.republishToPDT(requestId, userName, productCode);
  }

  @Override
  public List<ProductDetailResponse> getProductDetailsByProductCodes(String requestId,
      String username, List<String> productCodes) throws Exception {
    return this.productServiceRepository
        .findProductDetailsByProductCodes(requestId, username, productCodes);
  }

  @Override
  public ProductWorkflowStatusResponse getWorkflowStatus(String productCode) throws Exception {
    return this.productWorkflowRepository.getWorkflowStatus(productCode);
  }
}
