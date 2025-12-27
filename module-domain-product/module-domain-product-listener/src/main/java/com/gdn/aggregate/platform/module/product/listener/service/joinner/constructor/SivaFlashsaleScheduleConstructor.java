package com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor;

import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaFlashsaleScheduleService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Component("ProductSivaFlashsaleScheduleConstructor")
public class SivaFlashsaleScheduleConstructor {

  @Autowired
  private SivaProductService sivaProductService;

  @Autowired
  private SivaFlashsaleScheduleService sivaFlashsaleScheduleService;

  @Autowired
  private TimeService timeService;

  public SivaFlashsaleSchedule toSivaFlashsaleSchedule(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getSchedule)
        .map(SivaFlashsaleSchedule::toId)
        .map(scheduleId -> {
          SivaFlashsaleSchedule sivaFlashsaleSchedule = Optional.of(scheduleId)
              .map(sivaFlashsaleScheduleService::getExistingSivaFlashsaleSchedule)
              .orElseGet(SivaFlashsaleSchedule::new);
          sivaFlashsaleSchedule.setTimestamp(flashsaleProduct.getTimestamp());
          sivaFlashsaleSchedule.setStart(ModuleProductUtil.getFlashsaleProductStart(flashsaleProduct));
          sivaFlashsaleSchedule.setEnd(ModuleProductUtil.getFlashsaleProductEnd(flashsaleProduct));
          sivaFlashsaleSchedule.setId(ModuleProductUtil.toSivaFlashsaleScheduleId(ModuleProductUtil.getFlashsaleProductStart(flashsaleProduct),ModuleProductUtil.getFlashsaleProductEnd(flashsaleProduct)));
          sivaFlashsaleSchedule.setLogo(ModuleProductUtil.getFlashsaleProductLogo(flashsaleProduct));
          sivaFlashsaleSchedule.setSubFlashsaleUrl(ModuleProductUtil.getFlashsaleProductSubFlashsaleUrl(flashsaleProduct));
          sivaFlashsaleSchedule.setBackground(ModuleProductUtil.getFlashsaleProductBackground(flashsaleProduct));
          sivaFlashsaleSchedule.setTimeBased(ModuleProductUtil.isFlashsaleProductTimeBased(flashsaleProduct));
          sivaFlashsaleSchedule.setMarkForDelete(false);
          return sivaFlashsaleSchedule;
        })
        .orElse(null);
  }

  public SivaFlashsaleSchedule toSivaFlashsaleSchedule(FlashsaleSchedule flashsaleSchedule) {
    return Optional.ofNullable(flashsaleSchedule)
        .map(FlashsaleSchedule::getId)
        .map(sivaFlashsaleScheduleService::getExistingSivaFlashsaleSchedule)
        .map(sivaFlashsaleSchedule -> {
          sivaFlashsaleSchedule.setTimestamp(flashsaleSchedule.getTimestamp());
          sivaFlashsaleSchedule.setLogo(flashsaleSchedule.getLogo());
          sivaFlashsaleSchedule.setSubFlashsaleUrl(flashsaleSchedule.getSubFlashsaleUrl());
          sivaFlashsaleSchedule.setBackground(flashsaleSchedule.getBackground());
          sivaFlashsaleSchedule.setTimeBased(flashsaleSchedule.isTimeBased());
          sivaFlashsaleSchedule.setExpiryTime(timeService.getExpiryTime(sivaFlashsaleSchedule.getEnd(),MainUtil.toFriendlyClassName(sivaFlashsaleSchedule)));
          return sivaFlashsaleSchedule;
        })
        .orElse(null);
  }

  public List<SivaFlashsaleSchedule> toCleanSivaFlashsaleSchedules(long timestamp) {
    return sivaFlashsaleScheduleService.getNotEndedSivaFlashsaleSchedules(timestamp)
        .stream()
        .map(sivaFlashsaleSchedule -> {
          sivaFlashsaleSchedule.setTimestamp(timestamp);
          sivaFlashsaleSchedule.setMarkForDelete(!sivaProductService.doesScheduleHaveProducts(sivaFlashsaleSchedule));
          return sivaFlashsaleSchedule;
        })
        .collect(Collectors.toList());
  }

  public SivaFlashsaleSchedule toDeactivateSivaFlashsaleSchedule(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getSchedule)
        .map(SivaFlashsaleSchedule::getId)
        .map(sivaFlashsaleScheduleService::getExistingSivaFlashsaleSchedule)
        .map(sivaFlashsaleSchedule -> {
          sivaFlashsaleSchedule.setTimestamp(flashsaleProduct.getTimestamp());
          sivaFlashsaleSchedule.setMarkForDelete(true);
          return sivaFlashsaleSchedule;
        })
        .orElse(null);
  }

}
