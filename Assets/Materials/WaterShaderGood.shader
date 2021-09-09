// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "WaterShaderGood"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_Smoothness("Smoothness", Float) = 1
		_WaterDepthMap("WaterDepthMap", 2D) = "white" {}
		_TerrainSizeInUnits("Terrain Size In Units", Vector) = (40,40,0,0)
		_TerrainPositionInUnits("Terrain Position In Units", Vector) = (0,0,0,0)
		_WaterDepthMapFalloff("Water Depth Map Falloff", Float) = 1
		_WaterDepthMapStrength("Water Depth Map Strength", Float) = 1
		_MaxDepthMapValue("Max Depth Map Value", Float) = 1
		_NormalMapTexture("Normal Map Texture", 2D) = "white" {}
		_HeigthMapTexture("Heigth Map Texture", 2D) = "white" {}
		_NormalMapStrength("Normal Map Strength", Float) = 1
		_NormalMapScale("Normal Map Scale", Float) = 1
		_NormalMapDeformStrength("Normal Map Deform Strength", Float) = 1
		_NormalMapStrengthVariationScale("Normal Map Strength Variation Scale", Range( 0 , 1)) = 0.5
		_NormalMapStrengthVariationBias("Normal Map Strength Variation Bias", Range( 0 , 1)) = 0.4771704
		_NormalRotationNoiseContrast("Normal Rotation Noise Contrast", Float) = 1.49
		_NormalRotationNoiseScale("Normal Rotation Noise Scale", Float) = 0
		_NormalRotationNoiseMidpoint("Normal Rotation Noise Midpoint", Float) = -0.67
		_NormalMapScrollSpeed("NormalMapScrollSpeed", Float) = 1
		_NormalDirection2("Normal Direction 2", Vector) = (0,0,0,0)
		_NormalDirection1("Normal Direction 1", Vector) = (0,0,0,0)
		_WaveMapScale("Wave Map Scale", Float) = 1
		_WaveWidthRatio("Wave Width Ratio", Float) = 5
		_WaveSpherizeStrength("Wave Spherize Strength", Float) = 0
		_WaveSpeed("Wave Speed", Float) = 0
		_WaveSpherizeCenter("Wave Spherize Center", Vector) = (0,0,0,0)
		_WaveStrength("Wave Strength", Float) = 1
		_WaveContrast("Wave Contrast", Range( 0 , 5)) = 0
		_WaveHeight("Wave Height", Float) = 0
		_WaveDepthFalloff("Wave Depth Falloff", Range( 0 , 1)) = 0
		_WaveDepthMinValue("Wave Depth Min Value", Float) = 0.3
		_WaveMaxOffset("Wave Max Offset", Float) = 0.4
		[ASEEnd]_WaveBreakedBounds("Wave Breaked Bounds", Vector) = (0,1,0,0)

		//_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 0.5
		//_TransStrength( "Trans Strength", Range( 0, 50 ) ) = 1
		//_TransNormal( "Trans Normal Distortion", Range( 0, 1 ) ) = 0.5
		//_TransScattering( "Trans Scattering", Range( 1, 50 ) ) = 2
		//_TransDirect( "Trans Direct", Range( 0, 1 ) ) = 0.9
		//_TransAmbient( "Trans Ambient", Range( 0, 1 ) ) = 0.1
		//_TransShadow( "Trans Shadow", Range( 0, 1 ) ) = 0.5
		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		_TessValue( "Max Tessellation", Range( 1, 32 ) ) = 16
		_TessMin( "Tess Min Distance", Float ) = 10
		_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" }
		Cull Back
		AlphaToMask Off
		HLSLINCLUDE
		#pragma target 2.0

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define TESSELLATION_ON 1
			#pragma require tessellation tessHW
			#pragma hull HullFunction
			#pragma domain DomainFunction
			#define ASE_DISTANCE_TESSELLATION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 80301

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#define ASE_NEEDS_FRAG_WORLD_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord : TEXCOORD0;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float2 _WaveBreakedBounds;
			float2 _NormalDirection1;
			float2 _NormalDirection2;
			float2 _WaveSpherizeCenter;
			float2 _TerrainPositionInUnits;
			float2 _TerrainSizeInUnits;
			float _NormalMapScale;
			float _WaveHeight;
			float _WaveMaxOffset;
			float _WaveDepthFalloff;
			float _WaveDepthMinValue;
			float _WaterDepthMapStrength;
			float _WaterDepthMapFalloff;
			float _MaxDepthMapValue;
			float _WaveStrength;
			float _WaveMapScale;
			float _NormalMapStrength;
			float _WaveWidthRatio;
			float _WaveSpeed;
			float _WaveSpherizeStrength;
			float _NormalMapStrengthVariationBias;
			float _NormalMapStrengthVariationScale;
			float _NormalMapDeformStrength;
			float _NormalRotationNoiseMidpoint;
			float _NormalRotationNoiseScale;
			float _NormalRotationNoiseContrast;
			float _NormalMapScrollSpeed;
			float _WaveContrast;
			float _Smoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _HeigthMapTexture;
			sampler2D _WaterDepthMap;
			sampler2D _NormalMapTexture;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float4 CalculateContrast( float contrastValue, float4 colorTarget )
			{
				float t = 0.5 * ( 1.0 - contrastValue );
				return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
			}
			//https://www.shadertoy.com/view/XdXGW8
			float2 GradientNoiseDir( float2 x )
			{
				const float2 k = float2( 0.3183099, 0.3678794 );
				x = x * k + k.yx;
				return -1.0 + 2.0 * frac( 16.0 * k * frac( x.x * x.y * ( x.x + x.y ) ) );
			}
			
			float GradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 i = floor( p );
				float2 f = frac( p );
				float2 u = f * f * ( 3.0 - 2.0 * f );
				return lerp( lerp( dot( GradientNoiseDir( i + float2( 0.0, 0.0 ) ), f - float2( 0.0, 0.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 0.0 ) ), f - float2( 1.0, 0.0 ) ), u.x ),
						lerp( dot( GradientNoiseDir( i + float2( 0.0, 1.0 ) ), f - float2( 0.0, 1.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 1.0 ) ), f - float2( 1.0, 1.0 ) ), u.x ), u.y );
			}
			
			float2 UnityGradientNoiseDir( float2 p )
			{
				p = fmod(p , 289);
				float x = fmod((34 * p.x + 1) * p.x , 289) + p.y;
				x = fmod( (34 * x + 1) * x , 289);
				x = frac( x / 41 ) * 2 - 1;
				return normalize( float2(x - floor(x + 0.5 ), abs( x ) - 0.5 ) );
			}
			
			float UnityGradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 ip = floor( p );
				float2 fp = frac( p );
				float d00 = dot( UnityGradientNoiseDir( ip ), fp );
				float d01 = dot( UnityGradientNoiseDir( ip + float2( 0, 1 ) ), fp - float2( 0, 1 ) );
				float d10 = dot( UnityGradientNoiseDir( ip + float2( 1, 0 ) ), fp - float2( 1, 0 ) );
				float d11 = dot( UnityGradientNoiseDir( ip + float2( 1, 1 ) ), fp - float2( 1, 1 ) );
				fp = fp * fp * fp * ( fp * ( fp * 6 - 15 ) + 10 );
				return lerp( lerp( d00, d01, fp.y ), lerp( d10, d11, fp.y ), fp.x ) + 0.5;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float3 ase_worldPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float4 appendResult9 = (float4(ase_worldPos.x , ase_worldPos.z , 0.0 , 0.0));
				float4 ProjectedUV10 = appendResult9;
				float2 normalizeResult171 = normalize( _NormalDirection1 );
				float4 temp_output_3_0_g12 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult171 ) ), 0.0 , 0.0 ));
				float cos6_g12 = cos( 0.2 );
				float sin6_g12 = sin( 0.2 );
				float2 rotator6_g12 = mul( (temp_output_3_0_g12*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g12 , -sin6_g12 , sin6_g12 , cos6_g12 )) + float2( 0,0 );
				float simplePerlin2D92 = snoise( ProjectedUV10.xy*_NormalRotationNoiseScale );
				simplePerlin2D92 = simplePerlin2D92*0.5 + 0.5;
				float4 temp_cast_4 = (( simplePerlin2D92 + _NormalRotationNoiseMidpoint )).xxxx;
				float4 NormalMapRotationNoise96 = saturate( CalculateContrast(_NormalRotationNoiseContrast,temp_cast_4) );
				float4 temp_output_39_0_g12 = NormalMapRotationNoise96;
				float lerpResult11_g12 = lerp( tex2Dlod( _HeigthMapTexture, float4( rotator6_g12, 0, 0.0) ).r , tex2Dlod( _HeigthMapTexture, float4( temp_output_3_0_g12.xy, 0, 0.0) ).r , temp_output_39_0_g12.x);
				float gradientNoise65 = GradientNoise(ProjectedUV10.xy,_NormalMapStrengthVariationScale);
				gradientNoise65 = gradientNoise65*0.5 + 0.5;
				float4 temp_cast_8 = (( gradientNoise65 + -0.67 )).xxxx;
				float4 temp_cast_9 = (( 1.0 - _NormalMapStrengthVariationBias )).xxxx;
				float4 temp_cast_10 = (( 1.0 + _NormalMapStrengthVariationBias )).xxxx;
				float4 NormalMapStrengthVariation66 = (temp_cast_9 + (CalculateContrast(1.49,temp_cast_8) - float4( 0,0,0,0 )) * (temp_cast_10 - temp_cast_9) / (float4( 1,1,1,1 ) - float4( 0,0,0,0 )));
				float4 StrengthVariationMask35_g12 = NormalMapStrengthVariation66;
				float2 normalizeResult172 = normalize( _NormalDirection2 );
				float4 temp_output_3_0_g13 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult172 ) ), 0.0 , 0.0 ));
				float cos6_g13 = cos( 0.2 );
				float sin6_g13 = sin( 0.2 );
				float2 rotator6_g13 = mul( (temp_output_3_0_g13*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g13 , -sin6_g13 , sin6_g13 , cos6_g13 )) + float2( 0,0 );
				float4 temp_output_39_0_g13 = float4( 0,0,0,0 );
				float lerpResult11_g13 = lerp( tex2Dlod( _HeigthMapTexture, float4( rotator6_g13, 0, 0.0) ).r , tex2Dlod( _HeigthMapTexture, float4( temp_output_3_0_g13.xy, 0, 0.0) ).r , temp_output_39_0_g13.x);
				float4 StrengthVariationMask35_g13 = NormalMapStrengthVariation66;
				float simplePerlin2D176 = snoise( (ProjectedUV10*2.0 + float4( ( _TimeParameters.x * float2( -0.4,0 ) ), 0.0 , 0.0 )).xy );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float NormalMapBlendTexture184 = simplePerlin2D176;
				float4 lerpResult182 = lerp( ( (-0.1 + (lerpResult11_g12 - 0.0) * (0.9 - -0.1) / (1.0 - 0.0)) * ( _NormalMapDeformStrength * StrengthVariationMask35_g12 ) ) , ( (-0.1 + (lerpResult11_g13 - 0.0) * (0.9 - -0.1) / (1.0 - 0.0)) * ( _NormalMapDeformStrength * StrengthVariationMask35_g13 ) ) , NormalMapBlendTexture184);
				float4 NormalHeightMap126 = lerpResult182;
				float2 temp_output_2_0_g10 = ProjectedUV10.xy;
				float2 temp_output_11_0_g10 = ( temp_output_2_0_g10 - _WaveSpherizeCenter );
				float dotResult12_g10 = dot( temp_output_11_0_g10 , temp_output_11_0_g10 );
				float2 temp_cast_21 = (( _WaveSpherizeStrength / 10000.0 )).xx;
				float2 temp_cast_22 = (( _TimeParameters.x * _WaveSpeed )).xx;
				float2 temp_output_251_0 = ( temp_output_2_0_g10 + ( temp_output_11_0_g10 * ( dotResult12_g10 * dotResult12_g10 * temp_cast_21 ) ) + temp_cast_22 );
				float4 appendResult248 = (float4(_WaveWidthRatio , 1.0 , 0.0 , 0.0));
				float2 temp_output_250_0 = ( (appendResult248).xy / _WaveMapScale );
				float gradientNoise285 = UnityGradientNoise((temp_output_251_0*temp_output_250_0 + 0.0),1.0);
				gradientNoise285 = gradientNoise285*0.5 + 0.5;
				float clampResult42 = clamp( tex2Dlod( _WaterDepthMap, float4( (ProjectedUV10*float4( ( 1.0 / _TerrainSizeInUnits ), 0.0 , 0.0 ) + float4( -( _TerrainPositionInUnits / _TerrainSizeInUnits ), 0.0 , 0.0 )).xy, 0, 0.0) ).r , 0.0 , _MaxDepthMapValue );
				float clampResult28 = clamp( ( pow( (0.0 + (clampResult42 - 0.0) * (1.0 - 0.0) / (_MaxDepthMapValue - 0.0)) , _WaterDepthMapFalloff ) * _WaterDepthMapStrength ) , 0.0 , 1.0 );
				float WaterDepthMap13 = clampResult28;
				float WaveWaterDepthMap293 = pow( (_WaveDepthMinValue + (WaterDepthMap13 - 0.0) * (1.0 - _WaveDepthMinValue) / (1.0 - 0.0)) , _WaveDepthFalloff );
				float WaveHeightMap229 = ( ( pow( gradientNoise285 , _WaveContrast ) * _WaveStrength ) * WaveWaterDepthMap293 );
				float4 appendResult299 = (float4(-( _WaveMaxOffset * WaveWaterDepthMap293 ) , 1.0 , 0.0 , 0.0));
				float4 CombinedHeightMap108 = ( ( NormalHeightMap126 * float4( float3(0,1,0) , 0.0 ) ) + ( ( WaveHeightMap229 * appendResult299 ) + _WaveHeight ) );
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = CombinedHeightMap108.xyz;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag ( VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float4 appendResult9 = (float4(WorldPosition.x , WorldPosition.z , 0.0 , 0.0));
				float4 ProjectedUV10 = appendResult9;
				float clampResult42 = clamp( tex2D( _WaterDepthMap, (ProjectedUV10*float4( ( 1.0 / _TerrainSizeInUnits ), 0.0 , 0.0 ) + float4( -( _TerrainPositionInUnits / _TerrainSizeInUnits ), 0.0 , 0.0 )).xy ).r , 0.0 , _MaxDepthMapValue );
				float clampResult28 = clamp( ( pow( (0.0 + (clampResult42 - 0.0) * (1.0 - 0.0) / (_MaxDepthMapValue - 0.0)) , _WaterDepthMapFalloff ) * _WaterDepthMapStrength ) , 0.0 , 1.0 );
				float WaterDepthMap13 = clampResult28;
				float WaveBreakerMap312 = saturate( (0.0 + (WaterDepthMap13 - _WaveBreakedBounds.x) * (1.0 - 0.0) / (_WaveBreakedBounds.y - _WaveBreakedBounds.x)) );
				float3 temp_cast_3 = (WaveBreakerMap312).xxx;
				
				float2 normalizeResult171 = normalize( _NormalDirection1 );
				float4 temp_output_3_0_g12 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult171 ) ), 0.0 , 0.0 ));
				float gradientNoise65 = GradientNoise(ProjectedUV10.xy,_NormalMapStrengthVariationScale);
				gradientNoise65 = gradientNoise65*0.5 + 0.5;
				float4 temp_cast_7 = (( gradientNoise65 + -0.67 )).xxxx;
				float4 temp_cast_8 = (( 1.0 - _NormalMapStrengthVariationBias )).xxxx;
				float4 temp_cast_9 = (( 1.0 + _NormalMapStrengthVariationBias )).xxxx;
				float4 NormalMapStrengthVariation66 = (temp_cast_8 + (CalculateContrast(1.49,temp_cast_7) - float4( 0,0,0,0 )) * (temp_cast_9 - temp_cast_8) / (float4( 1,1,1,1 ) - float4( 0,0,0,0 )));
				float4 StrengthVariationMask35_g12 = NormalMapStrengthVariation66;
				float4 temp_output_19_0_g12 = ( _NormalMapStrength * StrengthVariationMask35_g12 );
				float3 unpack20_g12 = UnpackNormalScale( tex2D( _NormalMapTexture, temp_output_3_0_g12.xy ), temp_output_19_0_g12.x );
				unpack20_g12.z = lerp( 1, unpack20_g12.z, saturate(temp_output_19_0_g12.x) );
				float cos6_g12 = cos( 0.2 );
				float sin6_g12 = sin( 0.2 );
				float2 rotator6_g12 = mul( (temp_output_3_0_g12*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g12 , -sin6_g12 , sin6_g12 , cos6_g12 )) + float2( 0,0 );
				float3 unpack24_g12 = UnpackNormalScale( tex2D( _NormalMapTexture, rotator6_g12 ), temp_output_19_0_g12.x );
				unpack24_g12.z = lerp( 1, unpack24_g12.z, saturate(temp_output_19_0_g12.x) );
				float simplePerlin2D92 = snoise( ProjectedUV10.xy*_NormalRotationNoiseScale );
				simplePerlin2D92 = simplePerlin2D92*0.5 + 0.5;
				float4 temp_cast_15 = (( simplePerlin2D92 + _NormalRotationNoiseMidpoint )).xxxx;
				float4 NormalMapRotationNoise96 = saturate( CalculateContrast(_NormalRotationNoiseContrast,temp_cast_15) );
				float4 temp_output_39_0_g12 = NormalMapRotationNoise96;
				float3 lerpResult21_g12 = lerp( unpack20_g12 , unpack24_g12 , temp_output_39_0_g12.xyz);
				float2 normalizeResult172 = normalize( _NormalDirection2 );
				float4 temp_output_3_0_g13 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult172 ) ), 0.0 , 0.0 ));
				float4 StrengthVariationMask35_g13 = NormalMapStrengthVariation66;
				float4 temp_output_19_0_g13 = ( _NormalMapStrength * StrengthVariationMask35_g13 );
				float3 unpack20_g13 = UnpackNormalScale( tex2D( _NormalMapTexture, temp_output_3_0_g13.xy ), temp_output_19_0_g13.x );
				unpack20_g13.z = lerp( 1, unpack20_g13.z, saturate(temp_output_19_0_g13.x) );
				float cos6_g13 = cos( 0.2 );
				float sin6_g13 = sin( 0.2 );
				float2 rotator6_g13 = mul( (temp_output_3_0_g13*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g13 , -sin6_g13 , sin6_g13 , cos6_g13 )) + float2( 0,0 );
				float3 unpack24_g13 = UnpackNormalScale( tex2D( _NormalMapTexture, rotator6_g13 ), temp_output_19_0_g13.x );
				unpack24_g13.z = lerp( 1, unpack24_g13.z, saturate(temp_output_19_0_g13.x) );
				float4 temp_output_39_0_g13 = float4( 0,0,0,0 );
				float3 lerpResult21_g13 = lerp( unpack20_g13 , unpack24_g13 , temp_output_39_0_g13.xyz);
				float simplePerlin2D176 = snoise( (ProjectedUV10*2.0 + float4( ( _TimeParameters.x * float2( -0.4,0 ) ), 0.0 , 0.0 )).xy );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float NormalMapBlendTexture184 = simplePerlin2D176;
				float3 lerpResult175 = lerp( lerpResult21_g12 , lerpResult21_g13 , NormalMapBlendTexture184);
				float3 NormalMap127 = lerpResult175;
				
				float3 Albedo = temp_cast_3;
				float3 Normal = NormalMap127;
				float3 Emission = 0;
				float3 Specular = 0.5;
				float Metallic = 0;
				float Smoothness = _Smoothness;
				float Occlusion = 1;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#endif
				half4 color = UniversalFragmentPBR(
					inputData, 
					Albedo, 
					Metallic, 
					Specular, 
					Smoothness, 
					Occlusion, 
					Emission, 
					Alpha);

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif

				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );

					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif

				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, WorldNormal ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif

				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif

				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif

				return color;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define TESSELLATION_ON 1
			#pragma require tessellation tessHW
			#pragma hull HullFunction
			#pragma domain DomainFunction
			#define ASE_DISTANCE_TESSELLATION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 80301

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float2 _WaveBreakedBounds;
			float2 _NormalDirection1;
			float2 _NormalDirection2;
			float2 _WaveSpherizeCenter;
			float2 _TerrainPositionInUnits;
			float2 _TerrainSizeInUnits;
			float _NormalMapScale;
			float _WaveHeight;
			float _WaveMaxOffset;
			float _WaveDepthFalloff;
			float _WaveDepthMinValue;
			float _WaterDepthMapStrength;
			float _WaterDepthMapFalloff;
			float _MaxDepthMapValue;
			float _WaveStrength;
			float _WaveMapScale;
			float _NormalMapStrength;
			float _WaveWidthRatio;
			float _WaveSpeed;
			float _WaveSpherizeStrength;
			float _NormalMapStrengthVariationBias;
			float _NormalMapStrengthVariationScale;
			float _NormalMapDeformStrength;
			float _NormalRotationNoiseMidpoint;
			float _NormalRotationNoiseScale;
			float _NormalRotationNoiseContrast;
			float _NormalMapScrollSpeed;
			float _WaveContrast;
			float _Smoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _HeigthMapTexture;
			sampler2D _WaterDepthMap;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float4 CalculateContrast( float contrastValue, float4 colorTarget )
			{
				float t = 0.5 * ( 1.0 - contrastValue );
				return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
			}
			//https://www.shadertoy.com/view/XdXGW8
			float2 GradientNoiseDir( float2 x )
			{
				const float2 k = float2( 0.3183099, 0.3678794 );
				x = x * k + k.yx;
				return -1.0 + 2.0 * frac( 16.0 * k * frac( x.x * x.y * ( x.x + x.y ) ) );
			}
			
			float GradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 i = floor( p );
				float2 f = frac( p );
				float2 u = f * f * ( 3.0 - 2.0 * f );
				return lerp( lerp( dot( GradientNoiseDir( i + float2( 0.0, 0.0 ) ), f - float2( 0.0, 0.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 0.0 ) ), f - float2( 1.0, 0.0 ) ), u.x ),
						lerp( dot( GradientNoiseDir( i + float2( 0.0, 1.0 ) ), f - float2( 0.0, 1.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 1.0 ) ), f - float2( 1.0, 1.0 ) ), u.x ), u.y );
			}
			
			float2 UnityGradientNoiseDir( float2 p )
			{
				p = fmod(p , 289);
				float x = fmod((34 * p.x + 1) * p.x , 289) + p.y;
				x = fmod( (34 * x + 1) * x , 289);
				x = frac( x / 41 ) * 2 - 1;
				return normalize( float2(x - floor(x + 0.5 ), abs( x ) - 0.5 ) );
			}
			
			float UnityGradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 ip = floor( p );
				float2 fp = frac( p );
				float d00 = dot( UnityGradientNoiseDir( ip ), fp );
				float d01 = dot( UnityGradientNoiseDir( ip + float2( 0, 1 ) ), fp - float2( 0, 1 ) );
				float d10 = dot( UnityGradientNoiseDir( ip + float2( 1, 0 ) ), fp - float2( 1, 0 ) );
				float d11 = dot( UnityGradientNoiseDir( ip + float2( 1, 1 ) ), fp - float2( 1, 1 ) );
				fp = fp * fp * fp * ( fp * ( fp * 6 - 15 ) + 10 );
				return lerp( lerp( d00, d01, fp.y ), lerp( d10, d11, fp.y ), fp.x ) + 0.5;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float3 ase_worldPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float4 appendResult9 = (float4(ase_worldPos.x , ase_worldPos.z , 0.0 , 0.0));
				float4 ProjectedUV10 = appendResult9;
				float2 normalizeResult171 = normalize( _NormalDirection1 );
				float4 temp_output_3_0_g12 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult171 ) ), 0.0 , 0.0 ));
				float cos6_g12 = cos( 0.2 );
				float sin6_g12 = sin( 0.2 );
				float2 rotator6_g12 = mul( (temp_output_3_0_g12*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g12 , -sin6_g12 , sin6_g12 , cos6_g12 )) + float2( 0,0 );
				float simplePerlin2D92 = snoise( ProjectedUV10.xy*_NormalRotationNoiseScale );
				simplePerlin2D92 = simplePerlin2D92*0.5 + 0.5;
				float4 temp_cast_4 = (( simplePerlin2D92 + _NormalRotationNoiseMidpoint )).xxxx;
				float4 NormalMapRotationNoise96 = saturate( CalculateContrast(_NormalRotationNoiseContrast,temp_cast_4) );
				float4 temp_output_39_0_g12 = NormalMapRotationNoise96;
				float lerpResult11_g12 = lerp( tex2Dlod( _HeigthMapTexture, float4( rotator6_g12, 0, 0.0) ).r , tex2Dlod( _HeigthMapTexture, float4( temp_output_3_0_g12.xy, 0, 0.0) ).r , temp_output_39_0_g12.x);
				float gradientNoise65 = GradientNoise(ProjectedUV10.xy,_NormalMapStrengthVariationScale);
				gradientNoise65 = gradientNoise65*0.5 + 0.5;
				float4 temp_cast_8 = (( gradientNoise65 + -0.67 )).xxxx;
				float4 temp_cast_9 = (( 1.0 - _NormalMapStrengthVariationBias )).xxxx;
				float4 temp_cast_10 = (( 1.0 + _NormalMapStrengthVariationBias )).xxxx;
				float4 NormalMapStrengthVariation66 = (temp_cast_9 + (CalculateContrast(1.49,temp_cast_8) - float4( 0,0,0,0 )) * (temp_cast_10 - temp_cast_9) / (float4( 1,1,1,1 ) - float4( 0,0,0,0 )));
				float4 StrengthVariationMask35_g12 = NormalMapStrengthVariation66;
				float2 normalizeResult172 = normalize( _NormalDirection2 );
				float4 temp_output_3_0_g13 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult172 ) ), 0.0 , 0.0 ));
				float cos6_g13 = cos( 0.2 );
				float sin6_g13 = sin( 0.2 );
				float2 rotator6_g13 = mul( (temp_output_3_0_g13*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g13 , -sin6_g13 , sin6_g13 , cos6_g13 )) + float2( 0,0 );
				float4 temp_output_39_0_g13 = float4( 0,0,0,0 );
				float lerpResult11_g13 = lerp( tex2Dlod( _HeigthMapTexture, float4( rotator6_g13, 0, 0.0) ).r , tex2Dlod( _HeigthMapTexture, float4( temp_output_3_0_g13.xy, 0, 0.0) ).r , temp_output_39_0_g13.x);
				float4 StrengthVariationMask35_g13 = NormalMapStrengthVariation66;
				float simplePerlin2D176 = snoise( (ProjectedUV10*2.0 + float4( ( _TimeParameters.x * float2( -0.4,0 ) ), 0.0 , 0.0 )).xy );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float NormalMapBlendTexture184 = simplePerlin2D176;
				float4 lerpResult182 = lerp( ( (-0.1 + (lerpResult11_g12 - 0.0) * (0.9 - -0.1) / (1.0 - 0.0)) * ( _NormalMapDeformStrength * StrengthVariationMask35_g12 ) ) , ( (-0.1 + (lerpResult11_g13 - 0.0) * (0.9 - -0.1) / (1.0 - 0.0)) * ( _NormalMapDeformStrength * StrengthVariationMask35_g13 ) ) , NormalMapBlendTexture184);
				float4 NormalHeightMap126 = lerpResult182;
				float2 temp_output_2_0_g10 = ProjectedUV10.xy;
				float2 temp_output_11_0_g10 = ( temp_output_2_0_g10 - _WaveSpherizeCenter );
				float dotResult12_g10 = dot( temp_output_11_0_g10 , temp_output_11_0_g10 );
				float2 temp_cast_21 = (( _WaveSpherizeStrength / 10000.0 )).xx;
				float2 temp_cast_22 = (( _TimeParameters.x * _WaveSpeed )).xx;
				float2 temp_output_251_0 = ( temp_output_2_0_g10 + ( temp_output_11_0_g10 * ( dotResult12_g10 * dotResult12_g10 * temp_cast_21 ) ) + temp_cast_22 );
				float4 appendResult248 = (float4(_WaveWidthRatio , 1.0 , 0.0 , 0.0));
				float2 temp_output_250_0 = ( (appendResult248).xy / _WaveMapScale );
				float gradientNoise285 = UnityGradientNoise((temp_output_251_0*temp_output_250_0 + 0.0),1.0);
				gradientNoise285 = gradientNoise285*0.5 + 0.5;
				float clampResult42 = clamp( tex2Dlod( _WaterDepthMap, float4( (ProjectedUV10*float4( ( 1.0 / _TerrainSizeInUnits ), 0.0 , 0.0 ) + float4( -( _TerrainPositionInUnits / _TerrainSizeInUnits ), 0.0 , 0.0 )).xy, 0, 0.0) ).r , 0.0 , _MaxDepthMapValue );
				float clampResult28 = clamp( ( pow( (0.0 + (clampResult42 - 0.0) * (1.0 - 0.0) / (_MaxDepthMapValue - 0.0)) , _WaterDepthMapFalloff ) * _WaterDepthMapStrength ) , 0.0 , 1.0 );
				float WaterDepthMap13 = clampResult28;
				float WaveWaterDepthMap293 = pow( (_WaveDepthMinValue + (WaterDepthMap13 - 0.0) * (1.0 - _WaveDepthMinValue) / (1.0 - 0.0)) , _WaveDepthFalloff );
				float WaveHeightMap229 = ( ( pow( gradientNoise285 , _WaveContrast ) * _WaveStrength ) * WaveWaterDepthMap293 );
				float4 appendResult299 = (float4(-( _WaveMaxOffset * WaveWaterDepthMap293 ) , 1.0 , 0.0 , 0.0));
				float4 CombinedHeightMap108 = ( ( NormalHeightMap126 * float4( float3(0,1,0) , 0.0 ) ) + ( ( WaveHeightMap229 * appendResult299 ) + _WaveHeight ) );
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = CombinedHeightMap108.xyz;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif
				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "Meta"
			Tags { "LightMode"="Meta" }

			Cull Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define TESSELLATION_ON 1
			#pragma require tessellation tessHW
			#pragma hull HullFunction
			#pragma domain DomainFunction
			#define ASE_DISTANCE_TESSELLATION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 80301

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_FRAG_WORLD_POSITION


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float2 _WaveBreakedBounds;
			float2 _NormalDirection1;
			float2 _NormalDirection2;
			float2 _WaveSpherizeCenter;
			float2 _TerrainPositionInUnits;
			float2 _TerrainSizeInUnits;
			float _NormalMapScale;
			float _WaveHeight;
			float _WaveMaxOffset;
			float _WaveDepthFalloff;
			float _WaveDepthMinValue;
			float _WaterDepthMapStrength;
			float _WaterDepthMapFalloff;
			float _MaxDepthMapValue;
			float _WaveStrength;
			float _WaveMapScale;
			float _NormalMapStrength;
			float _WaveWidthRatio;
			float _WaveSpeed;
			float _WaveSpherizeStrength;
			float _NormalMapStrengthVariationBias;
			float _NormalMapStrengthVariationScale;
			float _NormalMapDeformStrength;
			float _NormalRotationNoiseMidpoint;
			float _NormalRotationNoiseScale;
			float _NormalRotationNoiseContrast;
			float _NormalMapScrollSpeed;
			float _WaveContrast;
			float _Smoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _HeigthMapTexture;
			sampler2D _WaterDepthMap;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float4 CalculateContrast( float contrastValue, float4 colorTarget )
			{
				float t = 0.5 * ( 1.0 - contrastValue );
				return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
			}
			//https://www.shadertoy.com/view/XdXGW8
			float2 GradientNoiseDir( float2 x )
			{
				const float2 k = float2( 0.3183099, 0.3678794 );
				x = x * k + k.yx;
				return -1.0 + 2.0 * frac( 16.0 * k * frac( x.x * x.y * ( x.x + x.y ) ) );
			}
			
			float GradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 i = floor( p );
				float2 f = frac( p );
				float2 u = f * f * ( 3.0 - 2.0 * f );
				return lerp( lerp( dot( GradientNoiseDir( i + float2( 0.0, 0.0 ) ), f - float2( 0.0, 0.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 0.0 ) ), f - float2( 1.0, 0.0 ) ), u.x ),
						lerp( dot( GradientNoiseDir( i + float2( 0.0, 1.0 ) ), f - float2( 0.0, 1.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 1.0 ) ), f - float2( 1.0, 1.0 ) ), u.x ), u.y );
			}
			
			float2 UnityGradientNoiseDir( float2 p )
			{
				p = fmod(p , 289);
				float x = fmod((34 * p.x + 1) * p.x , 289) + p.y;
				x = fmod( (34 * x + 1) * x , 289);
				x = frac( x / 41 ) * 2 - 1;
				return normalize( float2(x - floor(x + 0.5 ), abs( x ) - 0.5 ) );
			}
			
			float UnityGradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 ip = floor( p );
				float2 fp = frac( p );
				float d00 = dot( UnityGradientNoiseDir( ip ), fp );
				float d01 = dot( UnityGradientNoiseDir( ip + float2( 0, 1 ) ), fp - float2( 0, 1 ) );
				float d10 = dot( UnityGradientNoiseDir( ip + float2( 1, 0 ) ), fp - float2( 1, 0 ) );
				float d11 = dot( UnityGradientNoiseDir( ip + float2( 1, 1 ) ), fp - float2( 1, 1 ) );
				fp = fp * fp * fp * ( fp * ( fp * 6 - 15 ) + 10 );
				return lerp( lerp( d00, d01, fp.y ), lerp( d10, d11, fp.y ), fp.x ) + 0.5;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float3 ase_worldPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float4 appendResult9 = (float4(ase_worldPos.x , ase_worldPos.z , 0.0 , 0.0));
				float4 ProjectedUV10 = appendResult9;
				float2 normalizeResult171 = normalize( _NormalDirection1 );
				float4 temp_output_3_0_g12 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult171 ) ), 0.0 , 0.0 ));
				float cos6_g12 = cos( 0.2 );
				float sin6_g12 = sin( 0.2 );
				float2 rotator6_g12 = mul( (temp_output_3_0_g12*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g12 , -sin6_g12 , sin6_g12 , cos6_g12 )) + float2( 0,0 );
				float simplePerlin2D92 = snoise( ProjectedUV10.xy*_NormalRotationNoiseScale );
				simplePerlin2D92 = simplePerlin2D92*0.5 + 0.5;
				float4 temp_cast_4 = (( simplePerlin2D92 + _NormalRotationNoiseMidpoint )).xxxx;
				float4 NormalMapRotationNoise96 = saturate( CalculateContrast(_NormalRotationNoiseContrast,temp_cast_4) );
				float4 temp_output_39_0_g12 = NormalMapRotationNoise96;
				float lerpResult11_g12 = lerp( tex2Dlod( _HeigthMapTexture, float4( rotator6_g12, 0, 0.0) ).r , tex2Dlod( _HeigthMapTexture, float4( temp_output_3_0_g12.xy, 0, 0.0) ).r , temp_output_39_0_g12.x);
				float gradientNoise65 = GradientNoise(ProjectedUV10.xy,_NormalMapStrengthVariationScale);
				gradientNoise65 = gradientNoise65*0.5 + 0.5;
				float4 temp_cast_8 = (( gradientNoise65 + -0.67 )).xxxx;
				float4 temp_cast_9 = (( 1.0 - _NormalMapStrengthVariationBias )).xxxx;
				float4 temp_cast_10 = (( 1.0 + _NormalMapStrengthVariationBias )).xxxx;
				float4 NormalMapStrengthVariation66 = (temp_cast_9 + (CalculateContrast(1.49,temp_cast_8) - float4( 0,0,0,0 )) * (temp_cast_10 - temp_cast_9) / (float4( 1,1,1,1 ) - float4( 0,0,0,0 )));
				float4 StrengthVariationMask35_g12 = NormalMapStrengthVariation66;
				float2 normalizeResult172 = normalize( _NormalDirection2 );
				float4 temp_output_3_0_g13 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult172 ) ), 0.0 , 0.0 ));
				float cos6_g13 = cos( 0.2 );
				float sin6_g13 = sin( 0.2 );
				float2 rotator6_g13 = mul( (temp_output_3_0_g13*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g13 , -sin6_g13 , sin6_g13 , cos6_g13 )) + float2( 0,0 );
				float4 temp_output_39_0_g13 = float4( 0,0,0,0 );
				float lerpResult11_g13 = lerp( tex2Dlod( _HeigthMapTexture, float4( rotator6_g13, 0, 0.0) ).r , tex2Dlod( _HeigthMapTexture, float4( temp_output_3_0_g13.xy, 0, 0.0) ).r , temp_output_39_0_g13.x);
				float4 StrengthVariationMask35_g13 = NormalMapStrengthVariation66;
				float simplePerlin2D176 = snoise( (ProjectedUV10*2.0 + float4( ( _TimeParameters.x * float2( -0.4,0 ) ), 0.0 , 0.0 )).xy );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float NormalMapBlendTexture184 = simplePerlin2D176;
				float4 lerpResult182 = lerp( ( (-0.1 + (lerpResult11_g12 - 0.0) * (0.9 - -0.1) / (1.0 - 0.0)) * ( _NormalMapDeformStrength * StrengthVariationMask35_g12 ) ) , ( (-0.1 + (lerpResult11_g13 - 0.0) * (0.9 - -0.1) / (1.0 - 0.0)) * ( _NormalMapDeformStrength * StrengthVariationMask35_g13 ) ) , NormalMapBlendTexture184);
				float4 NormalHeightMap126 = lerpResult182;
				float2 temp_output_2_0_g10 = ProjectedUV10.xy;
				float2 temp_output_11_0_g10 = ( temp_output_2_0_g10 - _WaveSpherizeCenter );
				float dotResult12_g10 = dot( temp_output_11_0_g10 , temp_output_11_0_g10 );
				float2 temp_cast_21 = (( _WaveSpherizeStrength / 10000.0 )).xx;
				float2 temp_cast_22 = (( _TimeParameters.x * _WaveSpeed )).xx;
				float2 temp_output_251_0 = ( temp_output_2_0_g10 + ( temp_output_11_0_g10 * ( dotResult12_g10 * dotResult12_g10 * temp_cast_21 ) ) + temp_cast_22 );
				float4 appendResult248 = (float4(_WaveWidthRatio , 1.0 , 0.0 , 0.0));
				float2 temp_output_250_0 = ( (appendResult248).xy / _WaveMapScale );
				float gradientNoise285 = UnityGradientNoise((temp_output_251_0*temp_output_250_0 + 0.0),1.0);
				gradientNoise285 = gradientNoise285*0.5 + 0.5;
				float clampResult42 = clamp( tex2Dlod( _WaterDepthMap, float4( (ProjectedUV10*float4( ( 1.0 / _TerrainSizeInUnits ), 0.0 , 0.0 ) + float4( -( _TerrainPositionInUnits / _TerrainSizeInUnits ), 0.0 , 0.0 )).xy, 0, 0.0) ).r , 0.0 , _MaxDepthMapValue );
				float clampResult28 = clamp( ( pow( (0.0 + (clampResult42 - 0.0) * (1.0 - 0.0) / (_MaxDepthMapValue - 0.0)) , _WaterDepthMapFalloff ) * _WaterDepthMapStrength ) , 0.0 , 1.0 );
				float WaterDepthMap13 = clampResult28;
				float WaveWaterDepthMap293 = pow( (_WaveDepthMinValue + (WaterDepthMap13 - 0.0) * (1.0 - _WaveDepthMinValue) / (1.0 - 0.0)) , _WaveDepthFalloff );
				float WaveHeightMap229 = ( ( pow( gradientNoise285 , _WaveContrast ) * _WaveStrength ) * WaveWaterDepthMap293 );
				float4 appendResult299 = (float4(-( _WaveMaxOffset * WaveWaterDepthMap293 ) , 1.0 , 0.0 , 0.0));
				float4 CombinedHeightMap108 = ( ( NormalHeightMap126 * float4( float3(0,1,0) , 0.0 ) ) + ( ( WaveHeightMap229 * appendResult299 ) + _WaveHeight ) );
				
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = CombinedHeightMap108.xyz;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = MetaVertexPosition( v.vertex, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST, unity_DynamicLightmapST );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.texcoord1 = v.texcoord1;
				o.texcoord2 = v.texcoord2;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.texcoord2 = patch[0].texcoord2 * bary.x + patch[1].texcoord2 * bary.y + patch[2].texcoord2 * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 appendResult9 = (float4(WorldPosition.x , WorldPosition.z , 0.0 , 0.0));
				float4 ProjectedUV10 = appendResult9;
				float clampResult42 = clamp( tex2D( _WaterDepthMap, (ProjectedUV10*float4( ( 1.0 / _TerrainSizeInUnits ), 0.0 , 0.0 ) + float4( -( _TerrainPositionInUnits / _TerrainSizeInUnits ), 0.0 , 0.0 )).xy ).r , 0.0 , _MaxDepthMapValue );
				float clampResult28 = clamp( ( pow( (0.0 + (clampResult42 - 0.0) * (1.0 - 0.0) / (_MaxDepthMapValue - 0.0)) , _WaterDepthMapFalloff ) * _WaterDepthMapStrength ) , 0.0 , 1.0 );
				float WaterDepthMap13 = clampResult28;
				float WaveBreakerMap312 = saturate( (0.0 + (WaterDepthMap13 - _WaveBreakedBounds.x) * (1.0 - 0.0) / (_WaveBreakedBounds.y - _WaveBreakedBounds.x)) );
				float3 temp_cast_3 = (WaveBreakerMap312).xxx;
				
				
				float3 Albedo = temp_cast_3;
				float3 Emission = 0;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = Albedo;
				metaInput.Emission = Emission;
				
				return MetaFragment(metaInput);
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "Universal2D"
			Tags { "LightMode"="Universal2D" }

			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define TESSELLATION_ON 1
			#pragma require tessellation tessHW
			#pragma hull HullFunction
			#pragma domain DomainFunction
			#define ASE_DISTANCE_TESSELLATION
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 80301

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_2D

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#define ASE_NEEDS_FRAG_WORLD_POSITION


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float2 _WaveBreakedBounds;
			float2 _NormalDirection1;
			float2 _NormalDirection2;
			float2 _WaveSpherizeCenter;
			float2 _TerrainPositionInUnits;
			float2 _TerrainSizeInUnits;
			float _NormalMapScale;
			float _WaveHeight;
			float _WaveMaxOffset;
			float _WaveDepthFalloff;
			float _WaveDepthMinValue;
			float _WaterDepthMapStrength;
			float _WaterDepthMapFalloff;
			float _MaxDepthMapValue;
			float _WaveStrength;
			float _WaveMapScale;
			float _NormalMapStrength;
			float _WaveWidthRatio;
			float _WaveSpeed;
			float _WaveSpherizeStrength;
			float _NormalMapStrengthVariationBias;
			float _NormalMapStrengthVariationScale;
			float _NormalMapDeformStrength;
			float _NormalRotationNoiseMidpoint;
			float _NormalRotationNoiseScale;
			float _NormalRotationNoiseContrast;
			float _NormalMapScrollSpeed;
			float _WaveContrast;
			float _Smoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _HeigthMapTexture;
			sampler2D _WaterDepthMap;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float4 CalculateContrast( float contrastValue, float4 colorTarget )
			{
				float t = 0.5 * ( 1.0 - contrastValue );
				return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
			}
			//https://www.shadertoy.com/view/XdXGW8
			float2 GradientNoiseDir( float2 x )
			{
				const float2 k = float2( 0.3183099, 0.3678794 );
				x = x * k + k.yx;
				return -1.0 + 2.0 * frac( 16.0 * k * frac( x.x * x.y * ( x.x + x.y ) ) );
			}
			
			float GradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 i = floor( p );
				float2 f = frac( p );
				float2 u = f * f * ( 3.0 - 2.0 * f );
				return lerp( lerp( dot( GradientNoiseDir( i + float2( 0.0, 0.0 ) ), f - float2( 0.0, 0.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 0.0 ) ), f - float2( 1.0, 0.0 ) ), u.x ),
						lerp( dot( GradientNoiseDir( i + float2( 0.0, 1.0 ) ), f - float2( 0.0, 1.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 1.0 ) ), f - float2( 1.0, 1.0 ) ), u.x ), u.y );
			}
			
			float2 UnityGradientNoiseDir( float2 p )
			{
				p = fmod(p , 289);
				float x = fmod((34 * p.x + 1) * p.x , 289) + p.y;
				x = fmod( (34 * x + 1) * x , 289);
				x = frac( x / 41 ) * 2 - 1;
				return normalize( float2(x - floor(x + 0.5 ), abs( x ) - 0.5 ) );
			}
			
			float UnityGradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 ip = floor( p );
				float2 fp = frac( p );
				float d00 = dot( UnityGradientNoiseDir( ip ), fp );
				float d01 = dot( UnityGradientNoiseDir( ip + float2( 0, 1 ) ), fp - float2( 0, 1 ) );
				float d10 = dot( UnityGradientNoiseDir( ip + float2( 1, 0 ) ), fp - float2( 1, 0 ) );
				float d11 = dot( UnityGradientNoiseDir( ip + float2( 1, 1 ) ), fp - float2( 1, 1 ) );
				fp = fp * fp * fp * ( fp * ( fp * 6 - 15 ) + 10 );
				return lerp( lerp( d00, d01, fp.y ), lerp( d10, d11, fp.y ), fp.x ) + 0.5;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float3 ase_worldPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float4 appendResult9 = (float4(ase_worldPos.x , ase_worldPos.z , 0.0 , 0.0));
				float4 ProjectedUV10 = appendResult9;
				float2 normalizeResult171 = normalize( _NormalDirection1 );
				float4 temp_output_3_0_g12 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult171 ) ), 0.0 , 0.0 ));
				float cos6_g12 = cos( 0.2 );
				float sin6_g12 = sin( 0.2 );
				float2 rotator6_g12 = mul( (temp_output_3_0_g12*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g12 , -sin6_g12 , sin6_g12 , cos6_g12 )) + float2( 0,0 );
				float simplePerlin2D92 = snoise( ProjectedUV10.xy*_NormalRotationNoiseScale );
				simplePerlin2D92 = simplePerlin2D92*0.5 + 0.5;
				float4 temp_cast_4 = (( simplePerlin2D92 + _NormalRotationNoiseMidpoint )).xxxx;
				float4 NormalMapRotationNoise96 = saturate( CalculateContrast(_NormalRotationNoiseContrast,temp_cast_4) );
				float4 temp_output_39_0_g12 = NormalMapRotationNoise96;
				float lerpResult11_g12 = lerp( tex2Dlod( _HeigthMapTexture, float4( rotator6_g12, 0, 0.0) ).r , tex2Dlod( _HeigthMapTexture, float4( temp_output_3_0_g12.xy, 0, 0.0) ).r , temp_output_39_0_g12.x);
				float gradientNoise65 = GradientNoise(ProjectedUV10.xy,_NormalMapStrengthVariationScale);
				gradientNoise65 = gradientNoise65*0.5 + 0.5;
				float4 temp_cast_8 = (( gradientNoise65 + -0.67 )).xxxx;
				float4 temp_cast_9 = (( 1.0 - _NormalMapStrengthVariationBias )).xxxx;
				float4 temp_cast_10 = (( 1.0 + _NormalMapStrengthVariationBias )).xxxx;
				float4 NormalMapStrengthVariation66 = (temp_cast_9 + (CalculateContrast(1.49,temp_cast_8) - float4( 0,0,0,0 )) * (temp_cast_10 - temp_cast_9) / (float4( 1,1,1,1 ) - float4( 0,0,0,0 )));
				float4 StrengthVariationMask35_g12 = NormalMapStrengthVariation66;
				float2 normalizeResult172 = normalize( _NormalDirection2 );
				float4 temp_output_3_0_g13 = (ProjectedUV10*_NormalMapScale + float4( ( _TimeParameters.x * ( _NormalMapScrollSpeed * normalizeResult172 ) ), 0.0 , 0.0 ));
				float cos6_g13 = cos( 0.2 );
				float sin6_g13 = sin( 0.2 );
				float2 rotator6_g13 = mul( (temp_output_3_0_g13*1.3 + 0.0).xy - float2( 0,0 ) , float2x2( cos6_g13 , -sin6_g13 , sin6_g13 , cos6_g13 )) + float2( 0,0 );
				float4 temp_output_39_0_g13 = float4( 0,0,0,0 );
				float lerpResult11_g13 = lerp( tex2Dlod( _HeigthMapTexture, float4( rotator6_g13, 0, 0.0) ).r , tex2Dlod( _HeigthMapTexture, float4( temp_output_3_0_g13.xy, 0, 0.0) ).r , temp_output_39_0_g13.x);
				float4 StrengthVariationMask35_g13 = NormalMapStrengthVariation66;
				float simplePerlin2D176 = snoise( (ProjectedUV10*2.0 + float4( ( _TimeParameters.x * float2( -0.4,0 ) ), 0.0 , 0.0 )).xy );
				simplePerlin2D176 = simplePerlin2D176*0.5 + 0.5;
				float NormalMapBlendTexture184 = simplePerlin2D176;
				float4 lerpResult182 = lerp( ( (-0.1 + (lerpResult11_g12 - 0.0) * (0.9 - -0.1) / (1.0 - 0.0)) * ( _NormalMapDeformStrength * StrengthVariationMask35_g12 ) ) , ( (-0.1 + (lerpResult11_g13 - 0.0) * (0.9 - -0.1) / (1.0 - 0.0)) * ( _NormalMapDeformStrength * StrengthVariationMask35_g13 ) ) , NormalMapBlendTexture184);
				float4 NormalHeightMap126 = lerpResult182;
				float2 temp_output_2_0_g10 = ProjectedUV10.xy;
				float2 temp_output_11_0_g10 = ( temp_output_2_0_g10 - _WaveSpherizeCenter );
				float dotResult12_g10 = dot( temp_output_11_0_g10 , temp_output_11_0_g10 );
				float2 temp_cast_21 = (( _WaveSpherizeStrength / 10000.0 )).xx;
				float2 temp_cast_22 = (( _TimeParameters.x * _WaveSpeed )).xx;
				float2 temp_output_251_0 = ( temp_output_2_0_g10 + ( temp_output_11_0_g10 * ( dotResult12_g10 * dotResult12_g10 * temp_cast_21 ) ) + temp_cast_22 );
				float4 appendResult248 = (float4(_WaveWidthRatio , 1.0 , 0.0 , 0.0));
				float2 temp_output_250_0 = ( (appendResult248).xy / _WaveMapScale );
				float gradientNoise285 = UnityGradientNoise((temp_output_251_0*temp_output_250_0 + 0.0),1.0);
				gradientNoise285 = gradientNoise285*0.5 + 0.5;
				float clampResult42 = clamp( tex2Dlod( _WaterDepthMap, float4( (ProjectedUV10*float4( ( 1.0 / _TerrainSizeInUnits ), 0.0 , 0.0 ) + float4( -( _TerrainPositionInUnits / _TerrainSizeInUnits ), 0.0 , 0.0 )).xy, 0, 0.0) ).r , 0.0 , _MaxDepthMapValue );
				float clampResult28 = clamp( ( pow( (0.0 + (clampResult42 - 0.0) * (1.0 - 0.0) / (_MaxDepthMapValue - 0.0)) , _WaterDepthMapFalloff ) * _WaterDepthMapStrength ) , 0.0 , 1.0 );
				float WaterDepthMap13 = clampResult28;
				float WaveWaterDepthMap293 = pow( (_WaveDepthMinValue + (WaterDepthMap13 - 0.0) * (1.0 - _WaveDepthMinValue) / (1.0 - 0.0)) , _WaveDepthFalloff );
				float WaveHeightMap229 = ( ( pow( gradientNoise285 , _WaveContrast ) * _WaveStrength ) * WaveWaterDepthMap293 );
				float4 appendResult299 = (float4(-( _WaveMaxOffset * WaveWaterDepthMap293 ) , 1.0 , 0.0 , 0.0));
				float4 CombinedHeightMap108 = ( ( NormalHeightMap126 * float4( float3(0,1,0) , 0.0 ) ) + ( ( WaveHeightMap229 * appendResult299 ) + _WaveHeight ) );
				
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = CombinedHeightMap108.xyz;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float4 appendResult9 = (float4(WorldPosition.x , WorldPosition.z , 0.0 , 0.0));
				float4 ProjectedUV10 = appendResult9;
				float clampResult42 = clamp( tex2D( _WaterDepthMap, (ProjectedUV10*float4( ( 1.0 / _TerrainSizeInUnits ), 0.0 , 0.0 ) + float4( -( _TerrainPositionInUnits / _TerrainSizeInUnits ), 0.0 , 0.0 )).xy ).r , 0.0 , _MaxDepthMapValue );
				float clampResult28 = clamp( ( pow( (0.0 + (clampResult42 - 0.0) * (1.0 - 0.0) / (_MaxDepthMapValue - 0.0)) , _WaterDepthMapFalloff ) * _WaterDepthMapStrength ) , 0.0 , 1.0 );
				float WaterDepthMap13 = clampResult28;
				float WaveBreakerMap312 = saturate( (0.0 + (WaterDepthMap13 - _WaveBreakedBounds.x) * (1.0 - 0.0) / (_WaveBreakedBounds.y - _WaveBreakedBounds.x)) );
				float3 temp_cast_3 = (WaveBreakerMap312).xxx;
				
				
				float3 Albedo = temp_cast_3;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				half4 color = half4( Albedo, Alpha );

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				return color;
			}
			ENDHLSL
		}
		
	}
	/*ase_lod*/
	CustomEditor "UnityEditor.ShaderGraph.PBRMasterGUI"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=18900
0;72.44444;1167.889;571.8889;2416.771;-1249.387;1;True;False
Node;AmplifyShaderEditor.CommentaryNode;16;-7418.865,-571.4075;Inherit;False;664.0083;254.0788;ProjectUVFromAbove;3;8;9;10;;1,1,1,1;0;0
Node;AmplifyShaderEditor.WorldPosInputsNode;8;-7368.865,-521.4074;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.CommentaryNode;15;-6149.062,-1932.962;Inherit;False;2763.752;730.1431;WaterDepthMap;19;40;28;13;26;27;25;24;7;20;6;23;18;12;22;19;21;17;41;42;;1,1,1,1;0;0
Node;AmplifyShaderEditor.DynamicAppendNode;9;-7168.617,-497.2177;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector2Node;21;-5793.094,-1602.254;Inherit;False;Property;_TerrainPositionInUnits;Terrain Position In Units;4;0;Create;True;0;0;0;False;0;False;0,0;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.Vector2Node;17;-5969.176,-1390.372;Inherit;False;Property;_TerrainSizeInUnits;Terrain Size In Units;3;0;Create;True;0;0;0;False;0;False;40,40;40,40;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;19;-5966.775,-1484.855;Inherit;False;Constant;_Float0;Float 0;1;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;22;-5556.565,-1532.02;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;10;-6979.301,-476.0771;Inherit;False;ProjectedUV;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.NegateNode;23;-5417.567,-1553.02;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;18;-5759.936,-1472.306;Inherit;False;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;12;-6088.062,-1598.798;Inherit;False;10;ProjectedUV;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TexturePropertyNode;6;-5541.354,-1885.622;Inherit;True;Property;_WaterDepthMap;WaterDepthMap;2;0;Create;True;0;0;0;False;0;False;82bda4eb90f7aa543b07449b2bc0e898;82bda4eb90f7aa543b07449b2bc0e898;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.ScaleAndOffsetNode;20;-5283.303,-1721.084;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;1,0,0,0;False;2;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SamplerNode;7;-5057.393,-1800.942;Inherit;True;Property;_TextureSample0;Texture Sample 0;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;41;-4901.319,-1493.963;Inherit;False;Property;_MaxDepthMapValue;Max Depth Map Value;7;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;42;-4686.935,-1757.423;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;40;-4504.214,-1766.269;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;24;-4612.898,-1577.35;Inherit;False;Property;_WaterDepthMapFalloff;Water Depth Map Falloff;5;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;227;-6398.663,-438.0322;Inherit;False;3159.204;2493.825;Normal map;4;72;104;183;174;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;269;-2238.824,217.3005;Inherit;False;1761.029;888.0217;Noise Movement;18;270;271;272;285;243;251;250;249;264;255;233;265;231;254;235;253;248;245;;1,1,1,1;0;0
Node;AmplifyShaderEditor.PowerNode;25;-4265.881,-1735.237;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;27;-4316.326,-1584.983;Inherit;False;Property;_WaterDepthMapStrength;Water Depth Map Strength;6;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;72;-6348.663,1596.867;Inherit;False;1588.497;389.5402;NormalMapStrengthVariation;12;67;85;88;89;65;87;81;80;82;79;71;66;;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;26;-4093.849,-1723.889;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;245;-2001.561,916.8986;Inherit;False;Property;_WaveWidthRatio;Wave Width Ratio;30;0;Create;True;0;0;0;False;0;False;5;5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;104;-6061.494,-388.0322;Inherit;False;1467.957;420.207;NormalMapRotation;9;103;97;99;92;101;100;102;96;297;;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleTimeNode;235;-2203.125,530.6144;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;28;-3923.088,-1735.308;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;248;-1762.413,915.749;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;253;-2019.56,461.6853;Inherit;False;Property;_WaveSpherizeStrength;Wave Spherize Strength;31;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;71;-6301.301,1790.243;Inherit;False;Property;_NormalMapStrengthVariationScale;Normal Map Strength Variation Scale;16;0;Create;True;0;0;0;False;0;False;0.5;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;103;-5973.563,-338.0322;Inherit;False;10;ProjectedUV;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;97;-6011.494,-252.0233;Inherit;False;Property;_NormalRotationNoiseScale;Normal Rotation Noise Scale;19;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;79;-6293.427,1644.737;Inherit;False;10;ProjectedUV;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;254;-2218.1,626.7996;Inherit;False;Property;_WaveSpeed;Wave Speed;32;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;265;-1830.565,582.845;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;233;-1681.616,267.3005;Inherit;False;10;ProjectedUV;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CommentaryNode;294;497.7721,1116.571;Inherit;False;1212.647;339.3751;Wave water depth map;6;288;290;291;289;292;293;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;13;-3693.421,-1737.725;Inherit;False;WaterDepthMap;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;255;-1907.023,308.6412;Inherit;False;Property;_WaveSpherizeCenter;Wave Spherize Center;33;0;Create;True;0;0;0;False;0;False;0,0;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleDivideOpNode;264;-1721.145,469.1973;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;10000;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;249;-1615.411,785.5498;Inherit;False;True;True;False;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;65;-6074.538,1646.867;Inherit;False;Gradient;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;183;-4529.51,1524.126;Inherit;False;1240.051;535.7794;Normal Map Blend Texture;7;179;176;177;181;178;180;184;;1,1,1,1;0;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;92;-5718.609,-275.3029;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;99;-5796.119,-163.4733;Inherit;False;Property;_NormalRotationNoiseMidpoint;Normal Rotation Noise Midpoint;20;0;Create;True;0;0;0;False;0;False;-0.67;-0.67;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;89;-5980.276,1761.386;Inherit;False;Constant;_Midpoint;Midpoint;18;0;Create;True;0;0;0;False;0;False;-0.67;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;231;-1601.784,882.9637;Inherit;False;Property;_WaveMapScale;Wave Map Scale;29;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;288;547.7721,1174.784;Inherit;False;13;WaterDepthMap;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;174;-6237.657,159.7581;Inherit;False;2926.356;1300.996;Normal;32;127;126;153;138;133;140;157;134;58;143;156;165;150;50;139;148;131;158;129;47;164;168;169;141;172;171;142;170;167;175;182;185;;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;101;-5457.663,-266.4558;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;250;-1378.681,771.3396;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;251;-1471.079,467.4286;Inherit;False;Spherize;-1;;10;1488bb72d8899174ba0601b595d32b07;0;4;2;FLOAT2;0,0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;291;552.7844,1255.924;Inherit;False;Property;_WaveDepthMinValue;Wave Depth Min Value;39;0;Create;True;0;0;0;False;0;False;0.3;0.3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;85;-5649.174,1759.08;Inherit;False;Constant;_Contrastidk;Contrast? idk;17;0;Create;True;0;0;0;False;0;False;1.49;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;88;-5812.719,1648.91;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;180;-4351.51,1894.126;Inherit;False;Constant;_Vector1;Vector 1;24;0;Create;True;0;0;0;False;0;False;-0.4,0;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleTimeNode;179;-4479.51,1814.126;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;100;-5638.634,-83.04734;Inherit;False;Property;_NormalRotationNoiseContrast;Normal Rotation Noise Contrast;18;0;Create;True;0;0;0;False;0;False;1.49;1.49;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;82;-5719.527,1858.865;Inherit;False;Property;_NormalMapStrengthVariationBias;Normal Map Strength Variation Bias;17;0;Create;True;0;0;0;False;0;False;0.4771704;0.4771704;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleContrastOpNode;102;-5282.667,-283.5813;Inherit;False;2;1;COLOR;0,0,0,0;False;0;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;243;-1242.259,622.8774;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;1,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TFHCRemapNode;290;889.6035,1181.955;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleContrastOpNode;87;-5449.203,1649.416;Inherit;False;2;1;COLOR;0,0,0,0;False;0;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;80;-5385.528,1864.865;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;81;-5386.528,1765.866;Inherit;False;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;181;-4207.51,1750.126;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;178;-4271.509,1574.126;Inherit;False;10;ProjectedUV;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector2Node;167;-6187.657,716.9772;Inherit;False;Property;_NormalDirection1;Normal Direction 1;23;0;Create;True;0;0;0;False;0;False;0,0;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;292;942.3067,1363.931;Inherit;False;Property;_WaveDepthFalloff;Wave Depth Falloff;38;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;170;-6042.61,1313.454;Inherit;False;Property;_NormalDirection2;Normal Direction 2;22;0;Create;True;0;0;0;False;0;False;0,0;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.CommentaryNode;273;750.3696,543.6233;Inherit;False;1252.476;346.1032;Wave Height;5;263;262;266;267;229;;1,1,1,1;0;0
Node;AmplifyShaderEditor.PowerNode;289;1242.507,1176.2;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalizeNode;172;-5801.814,1305.603;Inherit;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SaturateNode;297;-5070.796,-294.6695;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;285;-937.3265,615.9937;Inherit;False;Gradient;True;True;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;177;-4079.51,1590.126;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT;2;False;2;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TFHCRemapNode;67;-5233.089,1646.545;Inherit;False;5;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;1,1,1,1;False;3;COLOR;0,0,0,0;False;4;COLOR;1,1,1,1;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;142;-6071.96,941.7685;Inherit;False;Property;_NormalMapScrollSpeed;NormalMapScrollSpeed;21;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NormalizeNode;171;-5974.742,704.0209;Inherit;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;280;-375.857,721.5946;Inherit;False;Property;_WaveContrast;Wave Contrast;36;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;168;-5810.834,652.343;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;293;1445.974,1166.571;Inherit;False;WaveWaterDepthMap;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;109;-1182.549,1975.291;Inherit;False;1301.335;571.1544;CombineHeightMaps;14;300;299;260;301;298;108;257;286;62;287;106;107;258;302;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;66;-5046.751,1645.526;Inherit;False;NormalMapStrengthVariation;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;96;-4917.165,-299.1335;Inherit;False;NormalMapRotationNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;169;-5628.143,1224.24;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleTimeNode;164;-5296.801,1157.57;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;263;800.3696,687.1154;Inherit;False;Property;_WaveStrength;Wave Strength;34;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;176;-3887.51,1574.126;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;283;-14.756,626.4102;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;141;-5910.119,549.42;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;129;-4946.564,248.3781;Inherit;False;10;ProjectedUV;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;139;-4631.29,442.0628;Inherit;False;Constant;_Float1;Float 1;20;0;Create;True;0;0;0;False;0;False;1.3;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;47;-5364.768,717.5496;Inherit;False;Property;_NormalMapStrength;Normal Map Strength;13;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;133;-5638.845,973.5525;Inherit;True;Property;_NormalMapTexture;Normal Map Texture;11;0;Create;True;0;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;262;1080.432,617.3983;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;301;-1173.057,2445.276;Inherit;False;293;WaveWaterDepthMap;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;298;-1161.751,2354.15;Inherit;False;Property;_WaveMaxOffset;Wave Max Offset;40;0;Create;True;0;0;0;False;0;False;0.4;0.4;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;267;1078.893,774.5043;Inherit;False;293;WaveWaterDepthMap;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;150;-4780.651,655.0035;Inherit;False;66;NormalMapStrengthVariation;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;148;-4974.96,695.79;Inherit;False;10;ProjectedUV;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;143;-5579.624,546.1483;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;184;-3614.951,1581.871;Inherit;False;NormalMapBlendTexture;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;50;-5378.476,819.374;Inherit;False;Property;_NormalMapScale;Normal Map Scale;14;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;58;-5273.521,636.3198;Inherit;False;Property;_NormalMapDeformStrength;Normal Map Deform Strength;15;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;158;-4656.387,870.2739;Inherit;False;Constant;_Float4;Float 4;20;0;Create;True;0;0;0;False;0;False;1.3;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;131;-4750.557,209.7582;Inherit;False;66;NormalMapStrengthVariation;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;156;-4865.272,377.433;Inherit;False;96;NormalMapRotationNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;157;-4736.114,951.925;Inherit;False;Constant;_Float3;Float 3;20;0;Create;True;0;0;0;False;0;False;0.2;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;134;-5660.828,746.647;Inherit;True;Property;_HeigthMapTexture;Heigth Map Texture;12;0;Create;True;0;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;140;-4636.126,518.6632;Inherit;False;Constant;_Float2;Float 2;20;0;Create;True;0;0;0;False;0;False;0.2;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;165;-4899.203,1156.395;Inherit;False;2;2;0;FLOAT;0.5;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;266;1380.989,618.9169;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;153;-4436.118,666.2086;Inherit;False;GetScrambledNormalsAndHeights;-1;;13;3a57150a09232fd41b6bc298573d7151;0;11;33;FLOAT4;0,0,0,0;False;38;FLOAT4;0,0,0,0;False;39;FLOAT4;0,0,0,0;False;32;FLOAT;0;False;29;FLOAT;0;False;30;FLOAT;0;False;43;FLOAT;0;False;40;FLOAT;0;False;31;FLOAT2;0,0;False;28;SAMPLER2D;0;False;27;SAMPLER2D;0;False;2;FLOAT4;26;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;300;-945.0212,2335.509;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;138;-4436.573,307.4023;Inherit;False;GetScrambledNormalsAndHeights;-1;;12;3a57150a09232fd41b6bc298573d7151;0;11;33;FLOAT4;0,0,0,0;False;38;FLOAT4;0,0,0,0;False;39;FLOAT4;0,0,0,0;False;32;FLOAT;0;False;29;FLOAT;0;False;30;FLOAT;0;False;43;FLOAT;0;False;40;FLOAT;0;False;31;FLOAT2;0,0;False;28;SAMPLER2D;0;False;27;SAMPLER2D;0;False;2;FLOAT4;26;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;185;-4067.637,683.08;Inherit;False;184;NormalMapBlendTexture;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;302;-820.5593,2263.843;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;229;1767.735,593.6233;Inherit;False;WaveHeightMap;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;182;-3828.755,399.9656;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;258;-906.9002,2151.075;Inherit;False;229;WaveHeightMap;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;126;-3602.035,431.8437;Inherit;False;NormalHeightMap;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;299;-687.5387,2263.7;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;260;-577.2838,2130.095;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;107;-1132.549,2025.291;Inherit;False;126;NormalHeightMap;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector2Node;313;-1932.264,1609.119;Inherit;False;Property;_WaveBreakedBounds;Wave Breaked Bounds;41;0;Create;True;0;0;0;False;0;False;0,1;0,1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.GetLocalVarNode;308;-2008.854,1495.723;Inherit;False;13;WaterDepthMap;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;287;-554.48,2260.856;Inherit;False;Property;_WaveHeight;Wave Height;37;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;106;-1100.799,2137.412;Inherit;False;Constant;_Vector0;Vector 0;21;0;Create;True;0;0;0;False;0;False;0,1,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;62;-883.868,2029.921;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleAddOpNode;286;-400.6493,2081.293;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.TFHCRemapNode;309;-1666.442,1557.464;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;257;-277.5279,2023.023;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CommentaryNode;225;-3121.407,-1988.492;Inherit;False;2045.526;1881.696;Albedo;5;209;210;38;207;222;;1,1,1,1;0;0
Node;AmplifyShaderEditor.SaturateNode;310;-1424.668,1495.122;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;222;-3036.175,-646.8482;Inherit;False;1153.307;336.5034;Depth Transparency Mask;6;221;217;220;215;216;223;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;207;-3046.523,-1190.458;Inherit;False;1729.663;332.1477;Refraction;8;208;204;203;201;206;202;306;307;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;38;-2851.53,-1938.492;Inherit;False;1534.621;572.8029;WaterColorRegular;8;39;36;34;37;33;303;304;305;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;228;1623.398,-517.542;Inherit;False;1364.167;798.6946;Result;14;191;190;173;52;51;276;46;56;214;232;212;14;224;315;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;108;-144.6466,2025.174;Inherit;False;CombinedHeightMap;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;312;-1263.159,1481.51;Inherit;False;WaveBreakerMap;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;14;1673.398,-392.0041;Inherit;False;39;WaterColorRegular;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;214;2076.457,-414.2049;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;1;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;56;2139.528,161.9305;Inherit;False;108;CombinedHeightMap;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;271;-1217.413,417.0657;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;1,0;False;2;FLOAT;2;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;173;2152.645,42.95266;Inherit;False;Property;_AlphaBase;Alpha Base;24;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;127;-3536.763,880.6487;Inherit;False;NormalMap;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;208;-2287.731,-973.006;Inherit;False;DistortionMask;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;224;1727.096,-291.6875;Inherit;False;223;DepthTransparencyMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;307;-2032.107,-1036.184;Inherit;False;Property;_UnderwaterDim;Underwater Dim;28;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;51;2004.41,-51.87035;Inherit;False;Property;_Metallic;Metallic;1;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;272;-651.3503,414.1035;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;52;2261.409,-63.86937;Inherit;False;Property;_Smoothness;Smoothness;0;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;209;-3059.407,-824.2854;Inherit;False;208;DistortionMask;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ScreenDepthNode;210;-2817.267,-840.7163;Inherit;False;0;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;212;1713.683,-467.5421;Inherit;False;204;Distortion;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;223;-2133.958,-580.3506;Inherit;False;DepthTransparencyMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;232;2031.617,-242.9515;Inherit;False;229;WaveHeightMap;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;276;2287.355,-366.3432;Inherit;False;Property;_Keyword0;Keyword 0;35;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;46;1968.798,-131.4705;Inherit;False;127;NormalMap;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;175;-3801.532,898.6661;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ClampOpNode;217;-2452.553,-590.0985;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;216;-3001.175,-509.0395;Inherit;False;Property;_TransparencyDepthDistance;Transparency Depth Distance;26;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;201;-2725.719,-1118.213;Inherit;False;DepthMaskedRefraction;-1;;14;c805f061214177c42bca056464193f81;2,40,0,103,0;2;35;FLOAT3;0,0,0;False;37;FLOAT;0.02;False;1;FLOAT2;38
Node;AmplifyShaderEditor.DepthFade;215;-2703.07,-596.8482;Inherit;False;True;False;True;2;1;FLOAT3;0,0,0;False;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;33;-2795.941,-1741.149;Inherit;False;Property;_WaterColorDeep;Water Color Deep;10;0;Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;202;-2996.523,-1140.458;Inherit;False;127;NormalMap;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;206;-2952.984,-1041.712;Inherit;False;Property;_RefractionStrength;Refraction Strength;25;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.VoronoiNode;270;-1000.903,395.994;Inherit;False;0;0;1;0;1;False;1;False;False;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;3;FLOAT;0;FLOAT2;1;FLOAT2;2
Node;AmplifyShaderEditor.RegisterLocalVarNode;39;-1650.833,-1738.141;Inherit;False;WaterColorRegular;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;204;-1578.204,-1095.041;Inherit;False;Distortion;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;220;-2543.524,-410.5426;Inherit;False;Property;_TransparencyDepthFalloff;Transparency Depth Falloff;27;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;221;-2310.99,-583.485;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;36;-1899.098,-1731.114;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;306;-1843.546,-1100.5;Inherit;False;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;37;-2099.271,-1522.852;Inherit;False;13;WaterDepthMap;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;315;2454.483,-192.9165;Inherit;False;312;WaveBreakerMap;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;304;-2424.205,-1820.458;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ScreenColorNode;203;-2278.115,-1140.502;Inherit;False;Global;_GrabScreen0;Grab Screen 0;24;0;Create;True;0;0;0;False;0;False;Object;-1;False;False;False;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;303;-2776.184,-1901.784;Inherit;False;Property;_WaterColorDeepTop;Water Color DeepTop;9;0;Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;34;-2304.179,-1667.088;Inherit;False;Property;_WaterColorShallow;Water Color Shallow;8;0;Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;305;-2687.081,-1573.252;Inherit;False;229;WaveHeightMap;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;191;2724.677,-188.4175;Float;False;True;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;WaterShaderGood;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;18;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;0;0;False;True;1;5;False;-1;10;False;-1;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;Hidden/InternalErrorShader;0;0;Standard;38;Workflow;1;Surface;1;  Refraction Model;0;  Blend;0;Two Sided;1;Fragment Normal Space,InvertActionOnDeselection;0;Transmission;0;  Transmission Shadow;0.5,False,-1;Translucency;0;  Translucency Strength;1,False,-1;  Normal Distortion;0.5,False,-1;  Scattering;2,False,-1;  Direct;0.9,False,-1;  Ambient;0.1,False,-1;  Shadow;0.5,False,-1;Cast Shadows;0;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;1;Built-in Fog;1;_FinalColorxAlpha;0;Meta Pass;1;Override Baked GI;0;Extra Pre Pass;0;DOTS Instancing;0;Tessellation;1;  Phong;0;  Strength;0.5,False,-1;  Type;1;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Write Depth;0;  Early Z;0;Vertex Position,InvertActionOnDeselection;1;0;6;False;True;False;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;192;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;193;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;194;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;190;2424.677,-178.4175;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;195;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;True;1;5;False;-1;10;False;-1;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;9;0;8;1
WireConnection;9;1;8;3
WireConnection;22;0;21;0
WireConnection;22;1;17;0
WireConnection;10;0;9;0
WireConnection;23;0;22;0
WireConnection;18;0;19;0
WireConnection;18;1;17;0
WireConnection;20;0;12;0
WireConnection;20;1;18;0
WireConnection;20;2;23;0
WireConnection;7;0;6;0
WireConnection;7;1;20;0
WireConnection;42;0;7;1
WireConnection;42;2;41;0
WireConnection;40;0;42;0
WireConnection;40;2;41;0
WireConnection;25;0;40;0
WireConnection;25;1;24;0
WireConnection;26;0;25;0
WireConnection;26;1;27;0
WireConnection;28;0;26;0
WireConnection;248;0;245;0
WireConnection;265;0;235;0
WireConnection;265;1;254;0
WireConnection;13;0;28;0
WireConnection;264;0;253;0
WireConnection;249;0;248;0
WireConnection;65;0;79;0
WireConnection;65;1;71;0
WireConnection;92;0;103;0
WireConnection;92;1;97;0
WireConnection;101;0;92;0
WireConnection;101;1;99;0
WireConnection;250;0;249;0
WireConnection;250;1;231;0
WireConnection;251;2;233;0
WireConnection;251;3;255;0
WireConnection;251;4;264;0
WireConnection;251;5;265;0
WireConnection;88;0;65;0
WireConnection;88;1;89;0
WireConnection;102;1;101;0
WireConnection;102;0;100;0
WireConnection;243;0;251;0
WireConnection;243;1;250;0
WireConnection;290;0;288;0
WireConnection;290;3;291;0
WireConnection;87;1;88;0
WireConnection;87;0;85;0
WireConnection;80;1;82;0
WireConnection;81;1;82;0
WireConnection;181;0;179;0
WireConnection;181;1;180;0
WireConnection;289;0;290;0
WireConnection;289;1;292;0
WireConnection;172;0;170;0
WireConnection;297;0;102;0
WireConnection;285;0;243;0
WireConnection;177;0;178;0
WireConnection;177;2;181;0
WireConnection;67;0;87;0
WireConnection;67;3;81;0
WireConnection;67;4;80;0
WireConnection;171;0;167;0
WireConnection;168;0;142;0
WireConnection;168;1;171;0
WireConnection;293;0;289;0
WireConnection;66;0;67;0
WireConnection;96;0;297;0
WireConnection;169;0;142;0
WireConnection;169;1;172;0
WireConnection;176;0;177;0
WireConnection;283;0;285;0
WireConnection;283;1;280;0
WireConnection;262;0;283;0
WireConnection;262;1;263;0
WireConnection;143;0;141;0
WireConnection;143;1;168;0
WireConnection;184;0;176;0
WireConnection;165;0;164;0
WireConnection;165;1;169;0
WireConnection;266;0;262;0
WireConnection;266;1;267;0
WireConnection;153;33;150;0
WireConnection;153;38;148;0
WireConnection;153;32;58;0
WireConnection;153;29;47;0
WireConnection;153;30;50;0
WireConnection;153;43;158;0
WireConnection;153;40;157;0
WireConnection;153;31;165;0
WireConnection;153;28;134;0
WireConnection;153;27;133;0
WireConnection;300;0;298;0
WireConnection;300;1;301;0
WireConnection;138;33;131;0
WireConnection;138;38;129;0
WireConnection;138;39;156;0
WireConnection;138;32;58;0
WireConnection;138;29;47;0
WireConnection;138;30;50;0
WireConnection;138;43;139;0
WireConnection;138;40;140;0
WireConnection;138;31;143;0
WireConnection;138;28;134;0
WireConnection;138;27;133;0
WireConnection;302;0;300;0
WireConnection;229;0;266;0
WireConnection;182;0;138;26
WireConnection;182;1;153;26
WireConnection;182;2;185;0
WireConnection;126;0;182;0
WireConnection;299;0;302;0
WireConnection;260;0;258;0
WireConnection;260;1;299;0
WireConnection;62;0;107;0
WireConnection;62;1;106;0
WireConnection;286;0;260;0
WireConnection;286;1;287;0
WireConnection;309;0;308;0
WireConnection;309;1;313;1
WireConnection;309;2;313;2
WireConnection;257;0;62;0
WireConnection;257;1;286;0
WireConnection;310;0;309;0
WireConnection;108;0;257;0
WireConnection;312;0;310;0
WireConnection;214;0;212;0
WireConnection;214;1;14;0
WireConnection;214;2;224;0
WireConnection;271;0;251;0
WireConnection;271;1;250;0
WireConnection;127;0;175;0
WireConnection;208;0;201;38
WireConnection;272;0;270;0
WireConnection;272;1;285;0
WireConnection;210;0;209;0
WireConnection;223;0;221;0
WireConnection;276;1;214;0
WireConnection;276;0;232;0
WireConnection;175;0;138;0
WireConnection;175;1;153;0
WireConnection;175;2;185;0
WireConnection;217;0;215;0
WireConnection;201;35;202;0
WireConnection;201;37;206;0
WireConnection;215;0;216;0
WireConnection;270;0;271;0
WireConnection;39;0;36;0
WireConnection;204;0;306;0
WireConnection;221;0;217;0
WireConnection;221;1;220;0
WireConnection;36;0;304;0
WireConnection;36;1;34;0
WireConnection;36;2;37;0
WireConnection;306;0;203;0
WireConnection;306;1;307;0
WireConnection;304;0;303;0
WireConnection;304;1;33;0
WireConnection;304;2;305;0
WireConnection;203;0;201;38
WireConnection;191;0;315;0
WireConnection;191;1;46;0
WireConnection;191;4;52;0
WireConnection;191;8;56;0
ASEEND*/
//CHKSM=6768E3D1673754B5271C0210C1CC057D8CAA5D5C