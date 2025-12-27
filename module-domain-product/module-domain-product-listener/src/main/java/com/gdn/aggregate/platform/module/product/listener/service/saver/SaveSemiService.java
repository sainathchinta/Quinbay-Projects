package com.gdn.aggregate.platform.module.product.listener.service.saver;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.properties.BatchProperties;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaCampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaFlashsaleGroupConstructor;
import com.gdn.aggregate.platform.module.product.listener.service.joinner.constructor.SivaFlashsaleScheduleConstructor;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaCampaignProductService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaFlashsaleGroupService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.semi.SivaFlashsaleScheduleService;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Component("ProductSaveSemiService")
public class SaveSemiService {

  @Autowired
  private BatchProperties batchProperties;

  @Autowired
  private SivaFlashsaleScheduleService sivaFlashsaleScheduleService;

  @Autowired
  private SivaFlashsaleGroupService sivaFlashsaleGroupService;

  @Autowired
  private SivaCampaignProductService sivaCampaignProductService;

  @Autowired
  private SivaFlashsaleScheduleConstructor sivaFlashsaleScheduleConstructor;

  @Autowired
  private SivaFlashsaleGroupConstructor sivaFlashsaleGroupConstructor;

  public Mono<Boolean> saveSivaFlashsaleSchedules(List<SivaFlashsaleSchedule> sivaFlashsaleSchedules, SaveParam saveParam) {
    if (CollectionUtils.isEmpty(sivaFlashsaleSchedules)) {
      return MainUtil.failedResult();
    } else if (CollectionUtils.size(sivaFlashsaleSchedules)==1) {
      return saveSivaFlashsaleSchedule(MainUtil.toListFirstData(sivaFlashsaleSchedules),saveParam);
    } else if (batchProperties.getSave().getEnable()) {
      sivaFlashsaleSchedules = sivaFlashsaleSchedules.stream()
          .filter(Objects::nonNull)
          .peek(sivaFlashsaleSchedule -> sivaFlashsaleScheduleService.setMandatory(sivaFlashsaleSchedule,saveParam))
          .collect(Collectors.toList());
      sivaFlashsaleScheduleService.setSaveParam(saveParam);
      return Flux.fromIterable(sivaFlashsaleSchedules)
          .buffer(batchProperties.getSave().getSize())
          .flatMap(vals -> sivaFlashsaleScheduleService.upsertMultiple(vals,saveParam))
          .reduce(MainUtil::reduce);
    } else {
      return Flux.fromIterable(sivaFlashsaleSchedules)
          .filter(Objects::nonNull)
          .flatMap(sivaFlashsaleSchedule -> saveSivaFlashsaleSchedule(sivaFlashsaleSchedule,saveParam))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> chainSaveSivaFlashsaleSchedule(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    if (Objects.isNull(flashsaleProduct)) {
      return MainUtil.failedResult();
    } else {
      SaveParam subSaveParam = ParamUtil.toSaveParamClearSourceClassName(saveParam);
      return saveSivaFlashsaleSchedule(sivaFlashsaleScheduleConstructor.toSivaFlashsaleSchedule(flashsaleProduct), subSaveParam);
    }
  }

  public Mono<Boolean> saveSivaFlashsaleSchedule(SivaFlashsaleSchedule sivaFlashsaleSchedule, SaveParam saveParam) {
    if (Objects.isNull(sivaFlashsaleSchedule)) {
      return MainUtil.failedResult();
    } else {
      sivaFlashsaleScheduleService.setSaveParam(saveParam);
      sivaFlashsaleScheduleService.setMandatory(sivaFlashsaleSchedule, saveParam);
      return sivaFlashsaleScheduleService.upsert(sivaFlashsaleSchedule, saveParam);
    }
  }

  public Mono<Boolean> saveSivaFlashsaleGroups(List<SivaFlashsaleGroup> sivaFlashsaleGroups, SaveParam saveParam) {
    if (CollectionUtils.isEmpty(sivaFlashsaleGroups)) {
      return MainUtil.failedResult();
    } else if (CollectionUtils.size(sivaFlashsaleGroups)==1) {
      return saveSivaFlashsaleGroup(MainUtil.toListFirstData(sivaFlashsaleGroups),saveParam);
    } else if (batchProperties.getSave().getEnable()) {
      sivaFlashsaleGroups = sivaFlashsaleGroups.stream()
          .filter(Objects::nonNull)
          .peek(sivaFlashsaleGroup -> sivaFlashsaleGroupService.setMandatory(sivaFlashsaleGroup,saveParam))
          .collect(Collectors.toList());
      sivaFlashsaleGroupService.setSaveParam(saveParam);
      return Flux.fromIterable(sivaFlashsaleGroups)
          .buffer(batchProperties.getSave().getSize())
          .flatMap(vals -> sivaFlashsaleGroupService.upsertMultiple(vals,saveParam))
          .reduce(MainUtil::reduce);
    } else {
      return Flux.fromIterable(sivaFlashsaleGroups)
          .filter(Objects::nonNull)
          .flatMap(sivaFlashsaleGroup -> saveSivaFlashsaleGroup(sivaFlashsaleGroup,saveParam))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> chainSaveSivaFlashsaleGroup(FlashsaleProduct flashsaleProduct, SaveParam saveParam) {
    if (Objects.isNull(flashsaleProduct)) {
      return MainUtil.failedResult();
    } else {
      SaveParam subSaveParam = ParamUtil.toSaveParamClearSourceClassName(saveParam);
      return saveSivaFlashsaleGroups(sivaFlashsaleGroupConstructor.toSivaFlashsaleGroups(flashsaleProduct), subSaveParam);
    }
  }

  public Mono<Boolean> saveSivaFlashsaleGroup(SivaFlashsaleGroup sivaFlashsaleGroup, SaveParam saveParam) {
    if (Objects.isNull(sivaFlashsaleGroup)) {
      return MainUtil.failedResult();
    } else {
      sivaFlashsaleGroupService.setSaveParam(saveParam);
      sivaFlashsaleGroupService.setMandatory(sivaFlashsaleGroup, saveParam);
      return sivaFlashsaleGroupService.upsert(sivaFlashsaleGroup, saveParam);
    }
  }

  public Mono<Boolean> saveSivaCampaignProducts(List<SivaCampaignProduct> sivaCampaignProducts, SaveParam saveParam) {
    if (CollectionUtils.isEmpty(sivaCampaignProducts)) {
      return MainUtil.failedResult();
    } else if (CollectionUtils.size(sivaCampaignProducts)==1) {
      return saveSivaCampaignProduct(MainUtil.toListFirstData(sivaCampaignProducts),saveParam);
    } else if (batchProperties.getSave().getEnable()) {
      sivaCampaignProducts = sivaCampaignProducts.stream()
          .filter(Objects::nonNull)
          .peek(sivaCampaignProduct -> sivaCampaignProductService.setMandatory(sivaCampaignProduct,saveParam))
          .collect(Collectors.toList());
      sivaCampaignProductService.setSaveParam(saveParam);
      return Flux.fromIterable(sivaCampaignProducts)
          .buffer(batchProperties.getSave().getSize())
          .flatMap(vals -> sivaCampaignProductService.upsertMultiple(vals,saveParam))
          .reduce(MainUtil::reduce);
    } else {
      return Flux.fromIterable(sivaCampaignProducts)
          .filter(Objects::nonNull)
          .flatMap(sivaCampaignProduct -> saveSivaCampaignProduct(sivaCampaignProduct,saveParam))
          .reduce(MainUtil::reduce);
    }
  }

  public Mono<Boolean> saveSivaCampaignProduct(SivaCampaignProduct sivaCampaignProduct, SaveParam saveParam) {
    if (Objects.isNull(sivaCampaignProduct)) {
      return MainUtil.failedResult();
    } else {
      sivaCampaignProductService.setSaveParam(saveParam);
      sivaCampaignProductService.setMandatory(sivaCampaignProduct, saveParam);
      return sivaCampaignProductService.upsert(sivaCampaignProduct, saveParam);
    }
  }

}
