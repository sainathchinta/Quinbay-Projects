package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.entity.ProductDataAutoFixHistory;
import com.gdn.mta.product.repository.ProductDataAutoFixHistoryRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductDataAutoFixHistoryServiceImpl implements ProductDataAutoFixHistoryService {

  @Autowired
  private ProductDataAutoFixHistoryRepository productDataAutoFixHistoryRepository;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveHistory(List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoList)
      throws ApplicationException {
    List<ProductDataAutoFixHistory> productDataAutoFixHistoryList = new ArrayList<>();
    if (!CollectionUtils.isEmpty(productDataAutoFixHistoryDtoList)) {
      for (ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto : productDataAutoFixHistoryDtoList) {
        ProductDataAutoFixHistory productDataAutoFixHistory = new ProductDataAutoFixHistory();
        BeanUtils.copyProperties(productDataAutoFixHistoryDto, productDataAutoFixHistory);
        GdnPreconditions
            .checkArgument(!StringUtils.isEmpty(productDataAutoFixHistory.getType()), Constants.TYPE_MUST_NOT_BE_EMPTY);
        productDataAutoFixHistoryList.add(productDataAutoFixHistory);
      }
      productDataAutoFixHistoryRepository.saveAll(productDataAutoFixHistoryList);
    } else {
      log.error("Empty history request found");
      throw new ApplicationException(ErrorCategory.INVALID_FORMAT);
    }
  }
}
