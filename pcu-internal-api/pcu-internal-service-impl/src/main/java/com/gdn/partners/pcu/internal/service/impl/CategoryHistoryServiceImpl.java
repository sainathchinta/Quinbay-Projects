package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.service.CategoryHistoryService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.model.response.CategoryHistoryWebResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
public class CategoryHistoryServiceImpl implements CategoryHistoryService {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public Page<CategoryHistoryWebResponse> categoryHistory(String storeId, String categoryCode,
      int page, int size) {
    GdnRestListResponse<CategoryHistoryResponse> response =
        pcbFeign.fetchCategoryHistory(categoryCode, page, size);
    ResponseHelper.validateMasterSkuResponse(response);
    List<CategoryHistoryWebResponse> responses =
        response.getContent().stream().map(ConverterUtil::toCategoryHistoryWebResponse)
            .collect(Collectors.toList());
    return new PageImpl<>(responses, PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }
}
