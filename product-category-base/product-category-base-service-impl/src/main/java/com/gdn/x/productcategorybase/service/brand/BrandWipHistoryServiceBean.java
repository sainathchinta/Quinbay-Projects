package com.gdn.x.productcategorybase.service.brand;

import java.util.List;

import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.repository.brand.BrandWipHistoryRepository;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@Transactional(readOnly = true)
public class BrandWipHistoryServiceBean implements BrandWipHistoryService {

  private static String HYPHEN = "-";

  @Autowired
  private BrandWipHistoryRepository brandWipHistoryRepository;

  @Async
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void generateBrandWipHistory(BrandWip brandWip, String additionalDescription, String username) {
    BrandWipHistory brandWipHistory = new BrandWipHistory();
    MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
    brandWipHistory.setBrandRequestCode(brandWip.getBrandRequestCode());
    brandWipHistory.setBrandCode(brandWip.getBrandCode());
    brandWipHistory.setState(brandWip.getState());
    brandWipHistory.setStoreId(GdnMandatoryParameterUtil.getStoreId());
    StringBuilder desc = new StringBuilder(brandWip.getState().getDescription());
    if (BrandWipState.REJECTED.equals(brandWip.getState())) {
      brandWipHistory.setDescription(
          desc.append(HYPHEN).append(additionalDescription).toString().getBytes());
    } else {
      if (StringUtils.isNoneEmpty(additionalDescription)) {
        desc.append(HYPHEN).append(additionalDescription);
        brandWipHistory.setDescription(desc.toString().getBytes());
      } else {
        brandWipHistory.setDescription(desc.toString().getBytes());
      }
    }
    this.brandWipHistoryRepository.save(brandWipHistory);
  }

  @Override
  public Page<BrandWipHistoryResponse> findByBrandRequestCode(String brandRequestCode, Pageable pageable) {
    Page<BrandWipHistory> page = brandWipHistoryRepository.findByBrandRequestCodeOrderByCreatedDateDesc(brandRequestCode, pageable);
    List<BrandWipHistoryResponse> responses = ConverterUtil.generateBrandWipHistoryResponses(page);
    return new PageImpl<>(responses);
  }

  @Override
  public Page<BrandWipHistoryResponse> findByBrandCode(String brandCode, Pageable pageable) {
    Page<BrandWipHistory> page = brandWipHistoryRepository.findByBrandCodeOrderByCreatedDateDesc(brandCode, pageable);
    List<BrandWipHistoryResponse> responses = ConverterUtil.generateBrandWipHistoryResponses(page);
    return new PageImpl<>(responses, pageable, page.getTotalElements());
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void saveBrandWipHistory(BrandHistoryEventModel brandHistoryEventModel) {
    BrandWipHistory brandWipHistory = new BrandWipHistory();
    BeanUtils.copyProperties(brandHistoryEventModel, brandWipHistory);
    brandWipHistoryRepository.save(brandWipHistory);
  }
}
