package com.gdn.x.mta.distributiontask.dao.api;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

public interface ProductCustomRepository {

  List<ProductBusinessPartnerMapperResponse> findByStoreIdAndKeywordAndStateAndUpdatedDate(String storeId,
      String keyword, List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, Pageable pageable,
      String vendorCode, Boolean brandPending);

  List<String> findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(String storeId,
      String keyword, List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, String vendorCode,
      Boolean brandPending);

  List<String> findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentPending(String storeId, String keyword,
      List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, String vendorCode,
      Boolean brandPending);

  List<String> findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateImagePending(String storeId, String keyword,
      List<WorkflowState> state, Date startDate, Date endDate, Boolean assignment, String vendorCode,
      Boolean brandPending);

  Page<Product> findProductByStoreIdAndKeywordAndStateAndUpdatedDateAndCategoryAndBusinessPartnerCodeAndAssigneeEmailId(
      String storeId, String keyword, List<WorkflowState> state, Date startDate, Date endDate,
      List<String> categoryCodes, String businessPartnerCode, String assigneeEmailId, String sortOrderByCreatedDate,
      Boolean assignment, Pageable pageable, String vendorCode, boolean postLive, String faultyImageType,
      Boolean brandPending);

}
