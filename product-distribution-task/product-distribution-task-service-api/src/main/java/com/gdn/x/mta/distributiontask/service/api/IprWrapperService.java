package com.gdn.x.mta.distributiontask.service.api;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.dto.BrandReport;
import com.gdn.x.mta.distributiontask.model.dto.IPRUpdateAssigneeRequest;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.dto.SubmitEvidenceRequest;

public interface IprWrapperService {

  /**
   *
   * @param productSku
   * @param storeId
   * @param source
   * @param assignee
   * @param brandReport
   * @return
   */
  String addProductToIPR(String productSku, String storeId, String source, String assignee,
      BrandReport brandReport) throws Exception;

  /**
   * update product status in ipr portal on listening product change event from x-product
   *
   * @param productChange ProductChange
   */
  void updateProductOnStateChange(ProductChange productChange) throws Exception;

  /**
   * Submit evidence for a product which is in evidence requested state
   *
   * @param submitEvidenceRequest SubmitEvidenceRequest
   */
  void submitEvidenceForProduct(SubmitEvidenceRequest submitEvidenceRequest)
      throws JsonProcessingException;

  /**
   * Update assignee for the IPR product
   *
   * @param iprUpdateAssigneeRequest
   */
  void updateAssignee(IPRUpdateAssigneeRequest iprUpdateAssigneeRequest) throws Exception;

  /**
   * to perform ipr actions - released, whitelisted, suspended and request evidence for product
   *
   * @param iprActionRequest IprActionRequest
   * @param storeId          String
   * @return error message if fail to update ipr products for any scenario
   */
  ErrorCategory performIprActionForProduct(IprActionRequest iprActionRequest, String storeId)
      throws Exception;

  /**
   * Suspend evidence requested product
   * @param productSku
   */
  void suspendEvidenceRequestedProduct(String productSku) throws Exception;

  /**
   * Adding IPR Products from DS Models
   *
   * @param productSku productSku
   * @param storeId storeId
   * @param source source
   */
  void addDSModelProductToIPR(String productSku, String storeId, String source);
}
