package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dto.DistributionFeaturedGoodsSettingSaveDTO;
import cn.lili.modules.distribution.entity.enums.DistributionFeaturedMatchRuleEnum;
import cn.lili.modules.distribution.entity.enums.DistributionFeaturedSourceModeEnum;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionFeaturedGoodsSetting;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 自定义主推商品保存按 displayCount 硬拦截。
 *
 * @author Mike
 * @date 2026-09-13
 */
class DistributionFeaturedGoodsServiceImplSaveSettingTest {

    private DistributionFeaturedGoodsServiceImpl service;
    private SettingService settingService;
    private DistributionGoodsService distributionGoodsService;

    /**
     * 初始化当前测试使用的MyBatis-Plus实体元数据。
     */
    @BeforeAll
    static void initMybatisEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, DistributionGoods.class);
    }

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        service = new DistributionFeaturedGoodsServiceImpl();
        settingService = mock(SettingService.class);
        distributionGoodsService = mock(DistributionGoodsService.class);
        ReflectionTestUtils.setField(service, "settingService", settingService);
        ReflectionTestUtils.setField(service, "distributionGoodsService", distributionGoodsService);
        when(settingService.getById(any())).thenReturn(null);
        when(settingService.saveUpdate(any(Setting.class))).thenReturn(true);
    }

    /**
     * 验证展示数量为五时拒绝超额自选商品。
     */
    @Test
    void saveSetting_rejectsCustomWhenSkuCountExceedsDisplayCountFive() {
        DistributionFeaturedGoodsSettingSaveDTO dto = customDto(5, skuIds(6));

        assertThatThrownBy(() -> service.saveSetting(dto))
                .isInstanceOf(ServiceException.class)
                .extracting(ex -> ((ServiceException) ex).getMsg())
                .asString()
                .contains("最多选择5个");
        verify(settingService, never()).saveUpdate(any(Setting.class));
    }

    /**
     * 验证展示数量为五时保存五个自选SKU。
     */
    @Test
    void saveSetting_persistsFiveCustomSkuIdsWhenDisplayCountIsFive() {
        List<String> skuIds = skuIds(5);
        stubPassedGoods(skuIds);

        service.saveSetting(customDto(5, skuIds));

        assertThat(savedSetting().getSkuIds()).containsExactlyElementsOf(skuIds);
    }

    /**
     * 验证展示数量为十时保存十个自选SKU。
     */
    @Test
    void saveSetting_persistsTenCustomSkuIdsWhenDisplayCountIsTen() {
        List<String> skuIds = skuIds(10);
        stubPassedGoods(skuIds);

        service.saveSetting(customDto(10, skuIds));

        assertThat(savedSetting().getSkuIds()).containsExactlyElementsOf(skuIds);
    }

    /**
     * 验证自选SKU去除空值及重复项后可以保存。
     */
    @Test
    void saveSetting_acceptsCustomAfterNormalizingBlankAndDuplicateSkuIds() {
        List<String> rawSkuIds = new ArrayList<>();
        rawSkuIds.add("sku-1");
        rawSkuIds.add("");
        rawSkuIds.add(" sku-1 ");
        rawSkuIds.add("sku-2");
        rawSkuIds.add("sku-3");
        rawSkuIds.add("sku-4");
        rawSkuIds.add("sku-5");
        stubPassedGoods(List.of("sku-1", "sku-2", "sku-3", "sku-4", "sku-5"));

        service.saveSetting(customDto(5, rawSkuIds));

        assertThat(savedSetting().getSkuIds()).containsExactly("sku-1", "sku-2", "sku-3", "sku-4", "sku-5");
    }

    /**
     * 验证自选模式未选择SKU时拒绝保存。
     */
    @Test
    void saveSetting_rejectsCustomWhenSkuIdsEmpty() {
        assertThatThrownBy(() -> service.saveSetting(customDto(5, new ArrayList<>())))
                .isInstanceOf(ServiceException.class)
                .extracting(ex -> ((ServiceException) ex).getMsg())
                .asString()
                .contains("请至少选择一个自定义主推商品");
        verify(settingService, never()).saveUpdate(any(Setting.class));
    }

    /**
     * 验证自动模式清除传入的自选SKU配置。
     */
    @Test
    void saveSetting_clearsSkuIdsInAutoModeEvenWhenMoreThanFiveSubmitted() {
        DistributionFeaturedGoodsSettingSaveDTO dto = new DistributionFeaturedGoodsSettingSaveDTO();
        dto.setSourceMode(DistributionFeaturedSourceModeEnum.AUTO.name());
        dto.setMatchRule(DistributionFeaturedMatchRuleEnum.HIGHEST_COMMISSION.name());
        dto.setDisplayCount(5);
        dto.setSkuIds(skuIds(8));

        service.saveSetting(dto);

        assertThat(savedSetting().getSkuIds()).isEmpty();
        verify(distributionGoodsService, never()).list(any(Wrapper.class));
    }

    /**
     * 验证SKU去重后仍超过展示数量时拒绝配置。
     */
    @Test
    void validateSetting_rejectsAfterDistinctCountExceedsDisplayCount() {
        DistributionFeaturedGoodsSettingSaveDTO dto = customDto(5, skuIds(6));
        dto.getSkuIds().add("sku-1");
        dto.getSkuIds().add(" ");

        assertThatThrownBy(() -> ReflectionTestUtils.invokeMethod(service, "validateSetting", dto))
                .isInstanceOf(ServiceException.class)
                .extracting(ex -> ((ServiceException) ex).getMsg())
                .asString()
                .contains("最多选择5个");
    }

    /**
     * 模拟已通过审核的商品及SKU查询结果。
     *
     * @param skuIds SKU ID列表
     */
    private void stubPassedGoods(List<String> skuIds) {
        List<DistributionGoods> goods = skuIds.stream().map(skuId -> {
            DistributionGoods item = new DistributionGoods();
            item.setSkuId(skuId);
            return item;
        }).collect(Collectors.toList());
        when(distributionGoodsService.list(any(Wrapper.class))).thenReturn(goods);
    }

    /**
     * 捕获服务保存的分销精选商品配置。
     *
     * @return 服务保存的分销精选商品配置
     */
    private DistributionFeaturedGoodsSetting savedSetting() {
        ArgumentCaptor<Setting> captor = ArgumentCaptor.forClass(Setting.class);
        verify(settingService).saveUpdate(captor.capture());
        return JSONUtil.toBean(captor.getValue().getSettingValue(), DistributionFeaturedGoodsSetting.class);
    }

    /**
     * 构造指定展示数量及SKU列表的自选商品配置。
     *
     * @param displayCount 展示数量
     * @param skuIds       SKU ID列表
     * @return 指定展示数量及SKU列表的自选商品配置
     */
    private static DistributionFeaturedGoodsSettingSaveDTO customDto(int displayCount, List<String> skuIds) {
        DistributionFeaturedGoodsSettingSaveDTO dto = new DistributionFeaturedGoodsSettingSaveDTO();
        dto.setSourceMode(DistributionFeaturedSourceModeEnum.CUSTOM.name());
        dto.setDisplayCount(displayCount);
        dto.setSkuIds(skuIds);
        return dto;
    }

    /**
     * 按指定数量生成测试SKU ID列表。
     *
     * @param count 数据数量
     * @return 生成的SKU ID列表
     */
    private static List<String> skuIds(int count) {
        return IntStream.rangeClosed(1, count)
                .mapToObj(i -> "sku-" + i)
                .collect(Collectors.toList());
    }
}
