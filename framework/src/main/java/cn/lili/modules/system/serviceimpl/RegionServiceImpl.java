package cn.lili.modules.system.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.common.utils.HttpClientUtils;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.system.entity.dos.Region;
import cn.lili.modules.system.entity.vo.RegionVO;
import cn.lili.modules.system.mapper.RegionMapper;
import cn.lili.modules.system.service.RegionService;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 行政地区业务层实现
 *
 * @author Chopper
 * @since 2020/12/2 11:11
 */
@Service
public class RegionServiceImpl extends ServiceImpl<RegionMapper, Region> implements RegionService {

    /**
     * 同步请求地址
     */
    private final String syncUrl = "https://restapi.amap.com/v3/config/district?subdistrict=4&key=e456d77800e2084a326f7b777278f89d";

    @Autowired
    private Cache cache;

    @Override
    public void synchronizationData(String url) {
        try {

            //清空数据
            QueryWrapper<Region> queryWrapper = new QueryWrapper<>();
            queryWrapper.ne("id", "-1");
            this.remove(queryWrapper);

            //读取数据
            String jsonString = HttpClientUtils.doGet(CharSequenceUtil.isEmpty(url) ? syncUrl : url, null);

            //构造存储数据库的对象集合
            List<Region> regions = this.initData(jsonString);
            for (int i = 0; i < (regions.size() / 100 + (regions.size() % 100 == 0 ? 0 : 1)); i++) {
                int endPoint = Math.min((100 + (i * 100)), regions.size());
                this.saveBatch(regions.subList(i * 100, endPoint));
            }
            //删除缓存
            cache.vagueDel("{regions}");
        } catch (Exception e) {
            log.error("同步行政数据错误", e);
        }
    }

    /**
     * 根据最后一级名称获取改所有上级地区id
     *
     * @param lastName 最后一级名称
     * @return 全部地区id
     */
    @Override
    public String getItemByLastName(String lastName) {
        StringBuilder sql = new StringBuilder();
        LambdaQueryWrapper<Region> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(Region::getName, lastName);
        Region region = this.getOne(lambdaQueryWrapper, false);
        if (region != null) {
            sql.append(region.getPath()).append(",").append(region.getId());
            return sql.toString().replace(",0,","");
        }
        return null;
    }

    @Override
    public List<Region> getItem(String id) {
        LambdaQueryWrapper<Region> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(Region::getParentId, id);
        List<Region> regions = this.list(lambdaQueryWrapper);
        regions.sort(Comparator.comparing(Region::getOrderNum));
        return regions;
    }

    @Override
    public Map<String, Object> getRegion(String cityCode, String townName) {
        //获取地址信息
        Region region = this.baseMapper.selectOne(new QueryWrapper<Region>()
                .eq("city_code", cityCode)
                .eq("name", townName));
        if (region != null) {
            //获取它的层级关系
            String path = region.getPath();
            String[] result = path.split(",");
            //因为有无用数据 所以先删除前两个
            result = ArrayUtils.remove(result, 0);
            result = ArrayUtils.remove(result, 0);
            //地址id
            StringBuilder regionIds = new StringBuilder();
            //地址名称
            StringBuilder regionNames = new StringBuilder();
            //循环构建新的数据
            for (String regionId : result) {
                Region reg = this.baseMapper.selectById(regionId);
                if (reg != null) {
                    regionIds.append(regionId).append(",");
                    regionNames.append(reg.getName()).append(",");
                }
            }
            regionIds.append(region.getId());
            regionNames.append(region.getName());
            //构建返回数据
            Map<String, Object> obj = new HashMap<>(2);
            obj.put("id", regionIds.toString());
            obj.put("name", regionNames.toString());
            return obj;
        }
        return Collections.emptyMap();
    }

    @Override
    public List<RegionVO> getAllCity() {
        LambdaQueryWrapper<Region> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        //查询所有省市
        lambdaQueryWrapper.in(Region::getLevel, "city", "province");
        return regionTree(this.list(lambdaQueryWrapper));
    }

    private List<RegionVO> regionTree(List<Region> regions) {
        List<RegionVO> regionVOS = new ArrayList<>();
        regions.stream().filter(region -> ("province").equals(region.getLevel())).forEach(item -> regionVOS.add(new RegionVO(item)));
        regions.stream().filter(region -> ("city").equals(region.getLevel())).forEach(item -> {
            for (RegionVO region : regionVOS) {
                if (region.getId().equals(item.getParentId())) {
                    region.getChildren().add(new RegionVO(item));
                }
            }
        });
        return regionVOS;
    }

    /**
     * 构造数据模型
     *
     * @param jsonString
     * @throws Exception
     */
    private List<Region> initData(String jsonString) {

        //最终数据承载对象
        List<Region> regions = new ArrayList<>();
        JSONObject jsonObject = JSONObject.parseObject(jsonString);
        //获取到国家及下面所有的信息 开始循环插入，这里可以写成递归调用，但是不如这样方便查看、理解
        JSONArray countryAll = jsonObject.getJSONArray("districts");
        for (int i = 0; i < countryAll.size(); i++) {
            JSONObject contry = countryAll.getJSONObject(i);
            String id1 = "0";
            JSONArray provinceAll = contry.getJSONArray("districts");
            for (int j = 0; j < provinceAll.size(); j++) {
                JSONObject province = provinceAll.getJSONObject(j);
                String citycode1 = province.getString("citycode");
                String adcode1 = province.getString("adcode");
                String name1 = province.getString("name");
                String center1 = province.getString("center");
                String level1 = province.getString("level");
                //插入省
                String id2 = insert(regions, id1, citycode1, adcode1, name1, center1, level1, j, id1);
                JSONArray cityAll = province.getJSONArray("districts");

                for (int z = 0; z < cityAll.size(); z++) {
                    JSONObject city = cityAll.getJSONObject(z);
                    String citycode2 = city.getString("citycode");
                    String adcode2 = city.getString("adcode");
                    String name2 = city.getString("name");
                    String center2 = city.getString("center");
                    String level2 = city.getString("level");
                    //插入市
                    String id3 = insert(regions, id2, citycode2, adcode2, name2, center2, level2, z, id1, id2);
                    JSONArray districtAll = city.getJSONArray("districts");
                    for (int w = 0; w < districtAll.size(); w++) {
                        JSONObject district = districtAll.getJSONObject(w);
                        String citycode3 = district.getString("citycode");
                        String adcode3 = district.getString("adcode");
                        String name3 = district.getString("name");
                        String center3 = district.getString("center");
                        String level3 = district.getString("level");
                        //插入区县
                        String id4 = insert(regions, id3, citycode3, adcode3, name3, center3, level3, w, id1, id2, id3);
                        //有需要可以继续向下遍历
                        JSONArray streetAll = district.getJSONArray("districts");
                        for (int r = 0; r < streetAll.size(); r++) {
                            JSONObject street = streetAll.getJSONObject(r);
                            String citycode4 = street.getString("citycode");
                            String adcode4 = street.getString("adcode");
                            String name4 = street.getString("name");
                            String center4 = street.getString("center");
                            String level4 = street.getString("level");
                            //插入区县
                            insert(regions, id4, citycode4, adcode4, name4, center4, level4, r, id1, id2, id3, id4);

                        }

                    }

                }
            }
        }
        return regions;
    }

    /**
     * 公共的插入方法
     *
     * @param parentId 父id
     * @param cityCode 城市编码
     * @param adCode   区域编码  街道没有独有的adcode，均继承父类（区县）的adcode
     * @param name     城市名称 （行政区名称）
     * @param center   地理坐标
     * @param level    country:国家
     *                 province:省份（直辖市会在province和city显示）
     *                 city:市（直辖市会在province和city显示）
     *                 district:区县
     *                 street:街道
     * @param ids      地区id集合
     * @return
     */
    public String insert(List<Region> regions, String parentId, String cityCode, String adCode, String name, String center, String level, Integer order, String... ids) {
//        \"citycode\": [],\n" +
//               "        \"adcode\": \"100000\",\n" +
//               "        \"name\": \"中华人民共和国\",\n" +
//               "        \"center\": \"116.3683244,39.915085\",\n" +
//               "        \"level\": \"country\",\n" +
        Region record = new Region();
        if (!("[]").equals(adCode)) {
            record.setAdCode(adCode);
        }
        if (!("[]").equals(cityCode)) {
            record.setCityCode(cityCode);
        }
        record.setCenter(center);
        record.setLevel(level);
        record.setName(name);
        record.setParentId(parentId);
        record.setOrderNum(order);
        record.setId(String.valueOf(SnowFlake.getId()));
        StringBuilder megName = new StringBuilder(",");
        for (int i = 0; i < ids.length; i++) {
            megName.append(ids[i]);
            if (i < ids.length - 1) {
                megName.append(",");
            }
        }
        record.setPath(megName.toString());
        regions.add(record);
        return record.getId();
    }

}