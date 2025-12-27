package com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.IdTimestamp;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaFlashsaleGroupService;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Component("ProductSivaFlashsaleGroupConstructor")
public class SivaFlashsaleGroupConstructor {

  @Autowired
  private SivaProductService sivaProductService;

  @Autowired
  private SivaFlashsaleGroupService sivaFlashsaleGroupService;

  public List<SivaFlashsaleGroup> toSivaFlashsaleGroups(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getGroupIds)
        .map(MainUtil::toCleanList)
        .orElseGet(ArrayList::new)
        .stream()
        .map(groupId -> {
          SivaFlashsaleGroup sivaFlashsaleGroup = Optional.of(groupId)
              .map(sivaFlashsaleGroupService::getExistingSivaFlashsaleGroup)
              .orElseGet(SivaFlashsaleGroup::new);
          sivaFlashsaleGroup.setTimestamp(flashsaleProduct.getTimestamp());
          sivaFlashsaleGroup.setGroupId(groupId);
          sivaFlashsaleGroup.setId(sivaFlashsaleGroup.toId());
          sivaFlashsaleGroup.setSchedules(toFixedScheduleSivaFlashsaleGroup(flashsaleProduct,sivaFlashsaleGroup));
          sivaFlashsaleGroup.setMarkForDelete(false);
          return sivaFlashsaleGroup;
        })
        .collect(Collectors.toList());
  }

  public SivaFlashsaleGroup toSivaFlashsaleGroup(FlashsaleGroup flashsaleGroup) {
    SivaFlashsaleGroup result = Optional.of(flashsaleGroup)
        .map(FlashsaleGroup::toId)
        .map(sivaFlashsaleGroupService::getExistingSivaFlashsaleGroup)
        .orElseGet(SivaFlashsaleGroup::new);
    BeanUtils.copyProperties(flashsaleGroup,result,"schedules");
    result.setId(result.toId());
    return result;
  }

  public SivaFlashsaleGroup toCleanSivaFlashsaleGroup(IdTimestamp idTimestamp) {
    return Optional.ofNullable(idTimestamp)
        .map(IdTimestamp::getId)
        .map(sivaFlashsaleGroupService::getExistingSivaFlashsaleGroup)
        .filter(val -> !CollectionUtils.isEmpty(val.getSchedules()))
        .map(val -> {
          List<SivaFlashsaleSchedule> sivaFlashsaleSchedules = val.getSchedules().stream()
              .filter(schedule -> sivaProductService.doesScheduleHaveProductsAndGroupId(schedule, val.getGroupId()))
              .collect(Collectors.toList());
          val.setTimestamp(idTimestamp.getTimestamp());
          val.setSchedules(sivaFlashsaleSchedules);
          return val;
        })
        .orElse(null);
  }

  public List<SivaFlashsaleGroup> toDeactivateSivaFlashsaleGroups(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .filter(val -> Objects.nonNull(val.getSchedule()))
        .filter(val -> !CollectionUtils.isEmpty(val.getGroupIds()))
        .map(FlashsaleProduct::getGroupIds)
        .map(MainUtil::fromListToSet)
        .map(sivaFlashsaleGroupService::getExistingSivaFlashsaleGroups)
        .map(vals -> vals.stream()
            .filter(Objects::nonNull)
            .filter(val -> !CollectionUtils.isEmpty(val.getSchedules()))
            .map(val -> {
              val.setTimestamp(flashsaleProduct.getTimestamp());
              val.setSchedules(toRemovedScheduleSivaFlashsaleGroup(flashsaleProduct,val));
              return val;
            })
            .collect(Collectors.toList()))
        .orElseGet(ArrayList::new);
  }

  private List<SivaFlashsaleSchedule> toRemovedScheduleSivaFlashsaleGroup(
      FlashsaleProduct flashsaleProduct, SivaFlashsaleGroup sivaFlashsaleGroup) {
    return Optional.ofNullable(sivaFlashsaleGroup.getSchedules())
        .map(vals -> sivaFlashsaleGroup.getSchedules().stream()
            .filter(Objects::nonNull)
            .filter(val -> !Optional.ofNullable(flashsaleProduct.getSchedule())
                .map(schedule -> schedule.getId().equals(val.getId()))
                .orElseGet(() -> false))
            .collect(Collectors.toList()))
        .orElseGet(ArrayList::new);
  }

  private List<SivaFlashsaleSchedule> toFixedScheduleSivaFlashsaleGroup(
      FlashsaleProduct flashsaleProduct, SivaFlashsaleGroup sivaFlashsaleGroup) {
    List<SivaFlashsaleSchedule> result = toRemovedScheduleSivaFlashsaleGroup(flashsaleProduct,sivaFlashsaleGroup);
    Optional.ofNullable(flashsaleProduct.getSchedule())
        .ifPresent(result::add);
    return result;
  }

}
