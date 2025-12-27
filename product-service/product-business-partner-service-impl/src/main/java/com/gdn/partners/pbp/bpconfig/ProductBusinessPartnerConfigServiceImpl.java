package com.gdn.partners.pbp.bpconfig;

import java.util.Calendar;
import java.util.Date;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;
import com.gdn.mta.product.entity.ProductBusinessPartnerConfig;
import com.gdn.mta.product.repository.ProductBusinessPartnerConfigRepository;
import com.gdn.partners.pbp.service.bpconfig.ProductBusinessPartnerConfigService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;

/**
 * Created by Vishal on 17/05/18.
 */

@Service
public class ProductBusinessPartnerConfigServiceImpl
    implements ProductBusinessPartnerConfigService {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(ProductBusinessPartnerConfigServiceImpl.class);


  @Autowired
  private ProductBusinessPartnerConfigRepository repository;

  @Lazy
  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Value("${default.mail.notify.reactivation.hours:48}")
  private int defaultMailOptionReactivationHours;

  @Override
  public boolean notifyMailVisibilityOptionForProductWip(String storeId, String bpCode) {
    boolean enableMailNotifyLink = Boolean.FALSE;
    try {
      Date currentDate = Calendar.getInstance().getTime();
      Integer count =
          productLevel3WipService.findCountByExceedingActivationDate(storeId, bpCode, currentDate);
      if (count > 0) {
        ProductBusinessPartnerConfig config = repository
            .findTopByStoreIdAndBpCodeOrderByProductToActivateNotifyMailDateDesc(storeId, bpCode);
        if (Objects.nonNull(config)) {
          Calendar cal = Calendar.getInstance();
          cal.setTime(currentDate);
          cal.add(Calendar.HOUR_OF_DAY, -defaultMailOptionReactivationHours);
          if (cal.getTime().after(config.getProductToActivateNotifyMailDate())) {
            enableMailNotifyLink = Boolean.TRUE;
          }
        } else {
          enableMailNotifyLink = Boolean.TRUE;
        }
      }
    } catch (Exception e) {
      LOGGER.error("failed to fetch to mail notify visibility option for BusinessPartner : {}",
          bpCode, e);
    }
    return enableMailNotifyLink;
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void save(String storeId, String username, ProductBusinessPartnerConfigRequest request) {
    ProductBusinessPartnerConfig config = new ProductBusinessPartnerConfig();
    BeanUtils.copyProperties(request, config);
    config.setStoreId(storeId);
    config.setCreatedBy(username);
    this.repository.save(config);
  }
}
