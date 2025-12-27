package com.gdn.x.mta.distributiontask.service.api;

import java.io.IOException;

import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AutoApprovalTypeRequestModel;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;

public interface AddProductToVendorService {

  /**
   * process ScreeingApprovalEvent
   * @param screeningProductApprovalEvent
   * @param prioritySeller
   */
  void processScreeningApprovalEvent(ScreeningProductApprovalEvent screeningProductApprovalEvent, int prioritySeller)
      throws Exception;


  /**
   * product AddEditedProductToPDTEvent
   * @param addEditedProductToPDTEvent
   */
  void processAddEditedProductEvent(AddEditedProductToPDTEvent addEditedProductToPDTEvent) throws Exception;

  /**
   *
   * @param addRevisedProductToPDTEvent
   */
  void processAddRevisedProductEvent(AddRevisedProductToPDTEvent addRevisedProductToPDTEvent) throws Exception;

  /**
   * PDT dimensions update
   *
   * @param pdtDimensionRefreshEventModel
   * @throws IOException
   */
  void processDimensionsUpdateEvent(PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel) throws IOException;

  /**
   *
   * @param autoApprovalTypeRequestModel
   */
  void processAutoApprovalCheckEvent(AutoApprovalTypeRequestModel autoApprovalTypeRequestModel);
}
